library(hexSticker)

p <- ggplot(aes(x = x, y = y), data = myDF) + geom_point(colour = "white")
p <- p + theme_void() + theme_transparent()
color = "deeppink"
sticker(p, package ="", p_color = "white", p_size = 12, h_fill = color, h_color = color, s_x=1, s_y=1, s_width=1.5, s_height = 1.5,
        filename=paste0(color,"_sticker.png"), url = "Data Driven Decision Making", u_color = "white", u_size = 2)
