corrs <- data.frame(cor(mtcars)[1,])
colnames(corrs) <- "Correlation_with_MPG"
corrs <- cbind("Variables" = rownames(corrs), corrs)
rownames(corrs) <- NULL
barplot(corrs$Correlation_with_MPG,
        names.arg = corrs$Variables,
        ylim = c(-1,1))
text(corrs$Correlation_with_MPG)

library(ggplot2)
cor_plot <- ggplot(data = corrs, 
                   mapping = aes(x = factor(Variables), 
                                 y = Correlation_with_MPG, 
                                 label = round(Correlation_with_MPG,2)))
cor_plot + 
    geom_bar(stat="Identity", color = "lightgrey") +
    geom_text(vjust = -0.5, color = "darkred") +
    theme_bw()