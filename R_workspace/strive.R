library("RColorBrewer")
library("plotly")
library("tidyverse")
library("gridExtra")
cu <- read.csv("/home/will/Documents/Work/data_science_notes/R_workspace/Strive1p10cu.csv",stringsAsFactors=FALSE,header=TRUE)
wr <- read.csv("/home/will/Documents/Work/data_science_notes/R_workspace/Strive1p10wr.csv",stringsAsFactors=FALSE,header=TRUE)
names(cu)[1] <- 'Floor'
names(wr)[1] <- 'Floor'
# In R/ggplot2, begin with a plot function - ggplot() - this creates the coordinate system
# then we add layers with +geom_point()
# these take mapping arguments which defines how the variables are mapped to visual elements
# mapping argument is always paired with =aes().  
labels <- colnames(wr)
labels <- labels[c(2:length(labels))]
charcolors <- c('blue','red','grey','purple','green','darkgoldenrod1','orangered','pink','navy','orange','navy','brown','deeppink','darkorange4','darkolivegreen','darkolivegreen4','firebrick1','black')
plot <- ggplot(data=wr, aes(x=Floor))+
  geom_line(aes(y=Anji),color=charcolors[1])+
  geom_line(aes(y=Axl),color=charcolors[2])+
  geom_line(aes(y=Chipp),color=charcolors[3])+
  geom_line(aes(y=Faust),color=charcolors[4])+
  geom_line(aes(y=Giovanna),color=charcolors[5])+
  geom_line(aes(y=Goldlewis),color=charcolors[6])+
  geom_line(aes(y=Ino),color=charcolors[7])+
  geom_line(aes(y=JackO),color=charcolors[8])+
  geom_line(aes(y=Ky),color=charcolors[9])+
  geom_line(aes(y=Leo),color=charcolors[10])+
  geom_line(aes(y=May),color=charcolors[11])+
  geom_line(aes(y=Millia),color=charcolors[12])+
  geom_line(aes(y=Nagoriyuki),color=charcolors[13])+
  geom_line(aes(y=Potemkin),color=charcolors[14])+
  geom_line(aes(y=Ramlethal),color=charcolors[15])+
  geom_line(aes(y=Sol),color=charcolors[16])+
  geom_line(aes(y=Zato),color=charcolors[17])+
  labs(color="Character",x="Floor",y="Win rate")+
  scale_color_manual(labels = labels, values = charcolors) +
  theme(legend.position = c(0, 1),legend.justification = c(0,1))
plot
wrt <- tibble(wr)
wrtpt1 <- wrt[1:3,1:18]
wrtpt2 <- wrt[4:6,1:18]
wrtpt3 <- wrt[7:8,1:18]
wrp1 <- wrtpt1 %>%
  pivot_longer(!Floor,names_to='characters',values_to='winrates')
wrp2 <- wrtpt2 %>%
  pivot_longer(!Floor,names_to='characters',values_to='winrates')
wrp3 <- wrtpt3 %>%
  pivot_longer(!Floor,names_to='characters',values_to='winrates')


# Some color definition stuff - will likely drop this for chosen colors
n <- 17
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
pie(rep(1,n), col=sample(col_vector, n))
colors = sample(col_vector,17)

# Plot:
p1 <- ggplot(data = wrp1) +
  geom_hline(yintercept=0.5, linetype="dotted", color = "white") + 
  geom_line(aes(Floor, winrates, color = characters)) +
  annotate("text", x = 4.1, y = wrp1$winrates[wrp1$Floor==4], label = wrp1$characters[wrp$Floor==4],color=colors) + 
  scale_color_manual(values =colors) +
  ggtitle('Floors 4-6') +
  scale_x_continuous(breaks=seq(4,6,1)) +
  theme(
    panel.background = element_rect(fill = "black",
                                    colour = "black",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.1, linetype = 'dashed',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.0, linetype = 'dashed',
                                    colour = "white"),
    legend.position="none",
    plot.background = element_rect(fill = "black"),
    axis.text = element_text(size = rel(1.2), colour = "white"),
    plot.title = element_text(colour = "white",hjust = 0.5,size=14)
  )
# Plot:
p2 <- ggplot(data = wrp2) +
  geom_hline(yintercept=0.5, linetype="dotted", color = "white") + 
  geom_line(aes(Floor, winrates, color = characters)) +
  annotate("text", x = 9.1, y = wrp2$winrates[wrp2$Floor==9], label = wrp1$characters[wrp2$Floor==9],color=colors) + 
  scale_color_manual(values =colors) +
  ggtitle('Floors 7-9') +
  scale_x_continuous(breaks=seq(7,9,1)) +
  theme(
    panel.background = element_rect(fill = "black",
                                    colour = "black",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.1, linetype = 'dashed',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.0, linetype = 'dashed',
                                    colour = "white"),
    legend.position="none",
    plot.background = element_rect(fill = "black"),
    axis.text = element_text(size = rel(1.2), colour = "white"),
    plot.title = element_text(colour = "white",hjust = 0.5,size=14)
  )
p3 <- ggplot(data = wrp3) +
  geom_hline(yintercept=0.5, linetype="dotted", color = "white") + 
  geom_line(aes(Floor, winrates, color = characters)) +
  annotate("text", x = 11, y = wrp3$winrates[wrp3$Floor==11], label = wrp3$characters[wrp3$Floor==11],color=colors) + 
  scale_color_manual(values =colors) +
  ggtitle('Floors 10 & Celestial') +
  scale_x_continuous(breaks=seq(10,11,1)) +
  theme(
    panel.background = element_rect(fill = "black",
                                    colour = "black",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.1, linetype = 'dashed',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.0, linetype = 'dashed',
                                    colour = "white"),
    legend.position="none",
    plot.background = element_rect(fill = "black"),
    axis.text = element_text(size = rel(1.2), colour = "white"),
    plot.title = element_text(colour = "white",hjust = 0.5,size=14)
  )
grid.arrange(p1,p2,p3,ncol=3)


