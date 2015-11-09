theme_MH <- function() {
  theme(
      axis.text.x=element_text(angle=90,size=22, colour = 'black'),
      axis.text.y=element_text(size=22, colour = 'black'),
      panel.background=element_rect(fill='grey95'),
      strip.background=element_rect(fill="#2c3e50"),
      panel.border = element_rect(colour = "#2c3e50", fill=NA, size=1),
      strip.text.x = element_text(colour = 'white', size = 22),
      legend.text=element_text(size=24),
      legend.title=element_blank(),
      panel.grid.major = element_line(colour = 'grey70', linetype = 'dashed'),
      panel.grid.minor = element_line(colour = 'grey70', linetype = 'dashed'))
  }


menu <- function(){
  fluidPage(
    wellPanel(
      helpText(h4('Selecciona el año que quieres ver.')),
      radioButtons(
        'filtroAnio', 
        label = '',
        choices = list(
          '2010' = '2010',
          '2012' = '2012',
          '2015' = '2015'),
        selected = c('2010'))
    )
  )}




