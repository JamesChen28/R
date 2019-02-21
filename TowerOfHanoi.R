# Play Tower of Hanoi
library(RColorBrewer)  # User-friendly color

P <- function(Play){
	if (Play == "y") {
		n <- readline("Please choose numbers of disks from 3 to 9: ")

		n <- as.numeric(n)
		if (is.na(n)){
			repeat{
				print("please input integer from 3 to 9")
				n <- readline("Please choose numbers of disks from 3 to 9: ")
				n <- as.numeric(n)
				if(!is.na(n)){
					break
				}
			}
		}
		if ((floor(n) == n) & (n >= 3) & (n <= 9)){
			run <- T
		}else{
			repeat{
				print("please input integer from 3 to 9")
				n <- readline("Please choose numbers of disks from 3 to 9: ")
				n <- as.numeric(n)
				if ((floor(n) == n) & (n >= 3) & (n <= 9)){
					break
				}
			}
		}  # In this paragraph, if users type words not 3 to 9, it will force you to type it.

		t <- list(A = c(1:n, n+1, n+2), B = c(n+1, n+2), C = c(n+1, n+2))  # List$A is the first stack.  B and C need 2 integers because the "if" function is not available if they are NULL. 
		bgcolor <- par("bg")
		draw.hanoi <- function(){
			cols <- brewer.pal(n = n, name = "Set1")
			par(mfrow = c(1, 3), mar = c(10, rep(0.2, 3)), cex.lab = 2)
			for (i in 1:3) {
				plot(c(-n, n), c(0, n + 2), type = "n", xlab = paste(LETTERS[i]), ylab = "", axes = FALSE)
				rect(-n, 0, n, n + 2, border = bgcolor, col = bgcolor)
				if (length(t[[i]][-((length(t[[i]])-1):length(t[[i]]))]) > 0) {
					barplot(rev(t[[i]][-((length(t[[i]])-1):length(t[[i]]))]), add = TRUE, horiz = TRUE,
						col = cols[rev(t[[i]][-((length(t[[i]])-1):length(t[[i]]))])], axes = F, border = NA, space = 0)
					barplot(-rev(t[[i]][-((length(t[[i]])-1):length(t[[i]]))]), add = TRUE, horiz = TRUE,
						col = cols[rev(t[[i]][-((length(t[[i]])-1):length(t[[i]]))])], axes = F, border = NA, space = 0)
					for (j in 1:length(t[[i]][-((length(t[[i]])-1):length(t[[i]]))])){
						text(x = 0, y = length(t[[i]][-((length(t[[i]])-1):length(t[[i]]))]) - j + 0.5,
							labels = paste(t[[i]][j]), col = "black", cex = 2)
					}
				}
			}
		}
		draw.hanoi()  # Draw the first stack by barplot.

		repeat{
			from <- readline("Please select a disk from A, B or C: ")
			if ((from == "A") | (from == "B") | (from == "C")){
				run <- T
			}else{
				repeat{
					print("please input A or B or C")
					from <- readline("Please select a disk from A, B or C: ")
					if ((from == "A") | (from == "B") | (from == "C")){
						break
					}
				}
			}
			to <- readline("Please select the disk to A, B or C: ")
			if ((to == "A") | (to == "B") | (to == "C")){
				run <- T
			}else{
				repeat{
					print("please input A or B or C")
					to <- readline("Please select the disk to A, B or C: ")
					if ((to == "A") | (to == "B") | (to == "C")){
						break
					}
				}
			}  # In this paragraph, if users type words not A or B or C, it will force you to type it.

			t[[to]] <- c(t[[from]][1], t[[to]])
			t[[from]] <- t[[from]][-1]  # These two lines mean the next condition after moving a disk

			if (t[[to]][1] >= t[[to]][2]){
				print("This is an illegal step!")
				t[[from]] <- c(t[[to]][1], t[[from]])
				t[[to]] <- t[[to]][2:length(t[[to]])]
			}else{
				draw.hanoi()
			}  # In this paragraph, if users move an illegal step, it will go to the previous condition

			x <- vector(mode = "logical", (n+2))
			for (i in 1:length(t[[2]])){
				x[i] <- (t[[2]][i] == i)
			}
			y <- vector(mode = "logical", (n+2))
			for (i in 1:length(t[[3]])){
				y[i] <- (t[[3]][i] == i)
			}
			if(is.na(summary(x)["TRUE"])){
				continue <- T
			}else{
				if(as.integer(summary(x[1:n])["TRUE"]) != n){
					continue <- T
				}else{
					print("Congratulations!")
					break
				}
			}
			if(is.na(summary(y)["TRUE"])){
				continue <- T
			}else{
				if(as.integer(summary(y[1:n])["TRUE"]) != n){
					continue <- T
				}else{
					print("Congratulations!")
					break
				}
			}  # In this paragraph, if users solve it at the end, the game will stop

		}
	}else{
		print("bye")
	}
}

P(readline("Do you want to play Tower of Hanoi? (y or n):"))

