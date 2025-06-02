#### External Requirements ####

### Libraries
library(rstatix)
library(ggpubr)
library(plyr)
library(tidyverse)
library(forcats)
library(ggpmisc)


### Read in CSV files

nerka<-read.csv("data/onerka_etoh.csv")
yp<-read.csv("data/yp_etoh.csv")
rbt<-read.csv("data/rbt_etoh.csv")
pks<-read.csv("data/pks_etoh.csv")
lmb<-read.csv("data/lmb_etoh.csv")
bgl<-read.csv("data/bgl_etoh.csv")
bbh<-read.csv("data/bbh_etoh.csv")

# Do you want to export resulsts?
# If so assign TRUE
# If not assign FALSE
export <- TRUE

#### Calculate Corrections ####

# Combine all dataframes into a single dataframe so it is easier to calculate
# the correction factors

combined.list <- list(
  nerka,
  yp,
  rbt,
  pks,
  lmb,
  bgl,
  bbh
)

combined.data <- bind_rows(combined.list)


# Pivot wider so that each time has its own column
combined.corr <- combined.data %>%  pivot_wider(id_cols = ID, names_from = time,
                        values_from = per.N:d13C,
                        names_prefix = "time.",
                        names_sep = ".") 

# Calculate the differences for each sample
# the values at time t - value at time 0
combined.corr <- combined.corr %>% 
  mutate(d13C.3m.corr = d13C.time.3 - d13C.time.0,
         d13C.6m.corr = d13C.time.6 - d13C.time.0,
         d13C.9m.corr = d13C.time.9 - d13C.time.0,
         d15N.3m.corr = d15N.time.3 - d15N.time.0,
         d15N.6m.corr = d15N.time.6 - d15N.time.0,
         d15N.9m.corr = d15N.time.9 - d15N.time.0,
         perC.3m.corr = per.C.time.3 - per.C.time.0,
         perC.6m.corr = per.C.time.6 - per.C.time.0,
         perC.9m.corr = per.C.time.9 - per.C.time.0,
         perN.3m.corr = per.N.time.3 - per.N.time.0,
         perN.6m.corr = per.N.time.6 - per.N.time.0,
         perN.9m.corr = per.N.time.9 - per.N.time.0)


# Divide back into species-specific tables

nerka.corr <- combined.corr %>% 
  filter(grepl("nerka", ID))

yp.corr <- combined.corr %>% 
  filter(grepl("yp", ID))

rbt.corr <- combined.corr %>% 
  filter(grepl("rbt", ID))

pks.corr <- combined.corr %>% 
  filter(grepl("pks", ID))

lmb.corr <- combined.corr %>% 
  filter(grepl("lmb", ID))

bgl.corr <- combined.corr %>% 
  filter(grepl("bgl", ID))

bbh.corr <- combined.corr %>% 
  filter(grepl("bbh", ID))

# Export to save as csv files

if(export){
  
  write.csv(nerka.corr,
            file = "outputs/corrections/nerka_diffs.csv")
  
  write.csv(yp.corr,
            file = "outputs/corrections/yp_diffs.csv")
  
  write.csv(rbt.corr,
            file = "outputs/corrections/rbt_diffs.csv")
  
  write.csv(pks.corr,
            file = "outputs/corrections/pks_diffs.csv")
  
  write.csv(lmb.corr,
            file = "outputs/corrections/lmb_diffs.csv")
  
  write.csv(bgl.corr,
            file = "outputs/corrections/bgl_diffs.csv")
  
  write.csv(bbh.corr,
            file = "outputs/corrections/bbh_diffs.csv")
  
}

# Find the mean and standard deviation for correction factors

combined.corr.sum <- combined.corr %>% 
  separate_wider_delim(cols = ID, delim = "_",
                       names = c("species", "ind.ID"),
                       cols_remove = FALSE)

# Summarize by species
combined.corr.sum <- combined.corr.sum %>% 
  dplyr::group_by(species) %>% 
  dplyr::summarise(d13C.3m = mean(d13C.3m.corr),
            d13C.3m.sd = sd(d13C.3m.corr),
            d13C.6m = mean(d13C.6m.corr),
            d13C.6m.sd = sd(d13C.6m.corr),
            d13C.9m = mean(d13C.9m.corr),
            d13C.9m.sd = sd(d13C.9m.corr),
            d15N.3m = mean(d15N.3m.corr),
            d15N.3m.sd = sd(d15N.3m.corr),
            d15N.6m = mean(d15N.6m.corr),
            d15N.6m.sd = sd(d15N.6m.corr),
            d15N.9m = mean(d15N.9m.corr),
            d15N.9m.sd = sd(d15N.9m.corr))

# Export results to csv for ease of access
if(export){
  
  write.csv(combined.corr.sum,
            file = "outputs/corrections/correction_factors.csv")
  
}

##### Format Data For Models ####
long.diffs <- combined.corr %>% 
  select(ID, d13C.3m.corr:perN.9m.corr) %>% 
  pivot_longer(cols = d13C.3m.corr:perN.9m.corr,
               names_to = c("variable", "month", "extra"),
               names_sep = "\\.",
               values_to = "diff") %>% 
  select(!extra)

long.diffs <- long.diffs %>% 
  separate_wider_delim(cols = ID, delim = "_",
                       names = c("species", "ind.ID"),
                       cols_remove = FALSE)

long.diffs <- long.diffs %>% 
  mutate(month.fct = factor(month, ordered = TRUE)) %>% 
  separate_wider_regex(month, patterns = c(time = "^\\d", extra = "m$")) %>% 
  mutate(time = as.numeric(time)) %>% 
  select(!extra)

# Both the time and the individual ID need to be factor variables
long.diffs <- long.diffs %>% 
  mutate(ind_ID = as.factor(ind.ID)) %>% 
  rename(month_fct = month.fct)


# Separate ID from species for plotting
combined.data <- combined.data %>% 
  separate_wider_delim(cols = ID, delim = "_",
                       names = c("species", "ind.ID"),
                       cols_remove = FALSE)

combined.corr <- combined.corr %>% 
  separate_wider_delim(cols = ID, delim = "_",
                       names = c("species", "ind.ID"),
                       cols_remove = FALSE)

#### RM ANOVA ####

##### Loop #####
# Make into a for-loop for each species and each variable

sps.of.interest <- unique(long.diffs$species)
variables <- c("d13C", "d15N")

# run model and Check assumptions
outlier.list <- list()
normality.list <- list()
rma.results.list <- list()

for(cur.taxa in sps.of.interest){
  for(cur.var in variables){
    
    mod.data <- long.diffs %>% 
      filter(species == cur.taxa,
             variable == cur.var) %>% 
      select(diff, ind_ID, month_fct)
    
    # Check for outliers
    outlier.list[[paste(cur.taxa, cur.var, sep = ".")]] <- mod.data %>% group_by(month_fct) %>% 
      identify_outliers(diff)
    
    # Check for normality
    normality.list[[paste(cur.taxa, cur.var, sep = ".")]] <- mod.data %>% group_by(month_fct) %>% 
      shapiro_test(diff)
    
    
    # Run the model 
    # Check for normality
    rma.results.list[[paste(cur.taxa, cur.var, sep = ".")]] <- anova_test(
      data = mod.data,
      dv = diff, # dependent variable
      wid = ind_ID, # factor indicating individual ID
      within = month_fct, # within-subject factor variable (i.e. time step)
      type = 3, # specifies repeated measures design
    )
    
  
  }
}

# Check outliers
outlier.list

# Check normality
normality.list

# See model results
rma.results.list

rma.table.list <- lapply(rma.results.list, get_anova_table, correction = "auto")

# Add the model names as a collumn to a dataset within a list
add.mod.name.column <- function(lst) {
  for (i in seq_along(lst)) {
    if(nrow(lst[[i]]) > 0){
      mod.name <- names(lst)[i]
      lst[[i]]$mod.name <- mod.name
    }
  }
  return(lst)
}

##### Turn into Dataframes ####

extract.rma.mod.results <- function(table.list){
  
  temp.data <- data.frame(Effect = table.list[["Effect"]],
                          DFn = table.list[["DFn"]],
                          DFd = table.list[["DFd"]],
                          F.stat = table.list[["F"]],
                          p.val = table.list[["p"]],
                          p.sig = if_else(table.list[["p<.05"]] == "*", TRUE, FALSE),
                          ges = table.list[["ges"]])
  
  return(temp.data)
  
}

# Outlier data
outlier.list <- add.mod.name.column(outlier.list)

outlier.data <- bind_rows(outlier.list)

extreme.outlier.data <- outlier.data %>% 
  filter(is.extreme)

# Normality Data
normality.list <- add.mod.name.column(normality.list)

normality.data <- bind_rows(normality.list)

normality.data <- normality.data %>% 
  rename(p.norm = p)

# Combine assumptions into a single data frame
assumption.data <- full_join(extreme.outlier.data, normality.data, by = c("mod.name", "month_fct"))

assumption.data <- assumption.data %>% select(mod.name, everything())

# Model Results
rma.table.list <- lapply(rma.table.list, as.data.frame)

extract.rma.mod.results(table.list = rma.table.list$nerka.d13C)

rma.model.list <- Map(extract.rma.mod.results, rma.table.list)

rma.model.list <- add.mod.name.column(rma.model.list)

rma.model.data <- bind_rows(rma.model.list)

if(export){
  
  write.csv(assumption.data, file = "outputs/model_results/rma_mod_assumptions.csv")
  
  write.csv(rma.model.data, file = "outputs/model_results/rma_mod_results.csv")
  
}

# Check QQ-plots 
violations <- normality.data %>% 
  filter(p.norm < 0.05) %>% 
  select(mod.name, everything()) %>% 
  separate_wider_delim(mod.name, delim = ".", 
                        names = c("species", "isotope"),
                        cols_remove = FALSE)

prob.mods <- violations$mod.name
prob.mnths <- violations$month_fct

# Create a bunch of qq plots to examine the data

qqlist <- list()

for(row in 1:length(violations$species)){

  mod.data <- long.diffs %>%
    filter(species == violations$species[row],
           variable == violations$isotope[row],
           month_fct == violations$month_fct[row]) %>%
    select(diff, ind.ID, month_fct)
  
  {
    qqnorm(mod.data$diff, main = paste(violations$species[row], violations$isotope[row], 
                                       violations$month_fct[row], sep = "."))
    qqline(mod.data$diff)
  }

}

qqlist

#### Pairwise t-test ####

head(rma.model.data)

# Find models that had significant results

sig.results <- rma.model.data %>% 
  filter(p.sig)

sig.results

# Now we need to get the species-variable combo so that we can filter based on
# that information

sig.results <- sig.results %>% 
  separate(col = mod.name, into = c("species", "variable"), sep = "\\.", remove = FALSE)

# Run the pairwise comparison for only the significant repeated measures ANOVA
pwc <- list()


for(i in 1:nrow(sig.results)){
  
    sig.data <- long.diffs %>% 
      filter(species == sig.results[i, "species"],
             variable == sig.results[i, "variable"]) %>% 
      select(diff, ind_ID, month_fct)
    
    pwc[[paste(sig.results[i, "mod.name"])]] <- pairwise_t_test(sig.data, 
                                                              diff ~ month_fct,
                                                              paired = TRUE,
                                                              p.adjust.method = "bonferroni")
    
    
}

pwc

# Turn list into a dataframe for easy export
pw.comp.list <- add.mod.name.column(pwc)

pw.comp.data <- bind_rows(pw.comp.list)

pw.comp.data <- pw.comp.data %>% 
  rename("y.var" = ".y.")

if(export){
  
  write.csv(pw.comp.data, file = "outputs/model_results/pairwise_comparison_results.csv")
  
}

#### Boxplots ####

# create boxplots to visualize the results

bxp.list <- list()

for(cur.taxa in sort(sps.of.interest)){
  for(cur.var in variables){
    
    mod.data <- long.diffs %>% 
      filter(species == cur.taxa,
             variable == cur.var) %>% 
      select(diff, ind_ID, month_fct)
    
    mod.name <- paste(cur.taxa, cur.var, sep = ".")
    
    if(mod.name %in% sig.results$mod.name){
      
      pw.comp.list[[paste(mod.name)]] <-  pw.comp.list[[paste(mod.name)]] %>% 
        add_xy_position(x = "month_fct")
      
      bxp.list[[mod.name]] <- ggboxplot(mod.data,
              x = "month_fct", y = "diff",
              add = "point",
              title = paste(cur.taxa))  +
        stat_pvalue_manual(pw.comp.list[[paste(mod.name)]],
                           step.increase = 0.1) +
        labs(x = "Time Point",
             y = ifelse(cur.var == "d13C",
                        expression(paste("Difference in ", 
                              delta^{13}, "C (\u2030)")),
                        expression(paste("Difference in ",
                                         delta^{15}, "N (\u2030)"))))
    } else {
      
      bxp.list[[mod.name]] <- ggboxplot(mod.data,
                x = "month_fct", y = "diff",
                add = "point",
                title = paste(cur.taxa)) +
        labs(x = "Time Point",
             y = ifelse(cur.var == "d13C",
                        expression(paste("Difference in ", 
                                         delta^{13}, "C (\u2030)")),
                        expression(paste("Difference in ",
                                         delta^{15}, "N (\u2030)"))))
      }
    
  }
}



sig.plots <- ggarrange(bxp.list[[3]] + rremove("xlab") + rremove("x.text") +
                         ggtitle("Bluegill"),
                       bxp.list[[4]] + rremove("xlab") + rremove("x.text") +
                         ggtitle("Bluegill"),
                       bxp.list[[1]] + rremove("xlab") + rremove("x.text") +
                         ggtitle("Brown Bullhead"),
                       bxp.list[[2]] + rremove("xlab") + rremove("x.text")+
                         ggtitle("Brown Bullhead"),
                       bxp.list[[5]] + rremove("xlab") + rremove("x.text") +
                         ggtitle("Largemouth Bass"),
                       bxp.list[[6]] + rremove("xlab") + rremove("x.text") +
                         ggtitle("Largemouth Bass"),
                       bxp.list[[11]] + ggtitle("Rainbow Trout"),
                       bxp.list[[12]] + ggtitle("Rainbow Trout"),
                       ncol = 2, nrow = 4)

sig.plots

if(export){
  ggsave(sig.plots, 
         filename = "outputs/figures/sig_boxplots.png",
         height = 11,
         width = 8,
         units = "in",
         dpi = 800)
}



#### Normal ANOVA ####
# test for difference from 0
anova.mod.list <- list()

for(cur.taxa in sps.of.interest){
  for(cur.var in variables){
    
    mod.data <- long.diffs %>% 
      filter(species == cur.taxa,
             variable == cur.var) %>% 
      select(diff, ind_ID, month_fct)
    
    # Set the model matrix to allow for group specific intercepts
    mod.mat <- model.matrix(~ mod.data$month_fct - 1)
    
    # run the model
    anova.mod.list[[paste(cur.taxa, cur.var, sep = ".")]] <- lm(diff ~ mod.mat - 1,
                                                                  data = mod.data)
    
    
  }
}

anova.summary.list <- lapply(anova.mod.list, summary)


extract.anova.mod.results <- function(summary.list){
  
  temp.data <- data.frame(Estimate = summary.list[["coefficients"]][,"Estimate"],
                          Std.err = summary.list[["coefficients"]][,"Std. Error"],
                          t.val = summary.list[["coefficients"]][,"t value"],
                          p.val = summary.list[["coefficients"]][,"Pr(>|t|)"],
                          p.sig = if_else(summary.list[["coefficients"]][,"Pr(>|t|)"] < 0.05, TRUE, FALSE))
  
  temp.data <- rownames_to_column(temp.data, var = "trt")
  return(temp.data)
  
}

#extract.anova.mod.results(anova.summary.list$nerka.d13C)

anova.results.list <- Map(extract.anova.mod.results, anova.summary.list)

anova.results.list <- add.mod.name.column(anova.results.list)

anova.results.data <- bind_rows(anova.results.list)

if(export){
  
  write.csv(anova.results.data, file = "outputs/model_results/anova_results.csv")
  
}

