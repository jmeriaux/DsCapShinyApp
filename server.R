
library("stringi")

otk_u <- readRDS('otk_u.rds');
otk_b <- readRDS('otk_b.rds');
otk_t <- readRDS('otk_t.rds');
otk_q <- readRDS('otk_q.rds');
top_u <- readRDS('top_u.rds');

#Values for parameters Lambdba (interpolation)
la0 <- 0.1;
la1 <- 0.3;
la2 <- 0.4;
la3 <- 0.2;

outp <- NULL;

###################################################################
# Extract 3 last token from string

lastgrams <- function( gram, proctext=TRUE )
{
  #Process text if proctext=TRUE (text is already processed for )
  if (proctext == TRUE) {   
    gram <- stri_replace_all_fixed(gram,"'","");
    gram <- unlist(stri_split_boundaries(gram, type="sentence"));  
    gram <- gsub(" [+-]?[0-9]?([,.-:0-9]*)?[.,]?[0-9]+"," zznumzz ",gram);  
    gram <- paste(stri_join("zzsebzz ", gram), collapse = "");
    wl <- unlist(stri_extract_all_words(tolower(gram )));
  }
  else
  { wl <- unlist(stri_extract_all_words(tolower(gram )));  }
  
  #Tokenize corpus
  w <- NULL;
  
  if (  length(wl) >= 3 )
  { 
    w[3] <- wl[length(wl)];
    w[2] <- wl[length(wl)-1];
    w[1] <- wl[length(wl)-2];
  }
  
  if ( length(wl) == 2 )
  { 
    w[3] <- wl[2];
    w[2] <- wl[1];
    w[1] <- "     ";
  }
  
  if ( length(wl) == 1 )
  { 
    w[3] <- wl[1];
    w[2] <- "     ";
    w[1] <- "     ";
  }
  
  return(w);
}

##############################################################
# Return predicted word for a given n-gram ( 1, 2 or 3 grams)
##############################################################

fd_nextw <- function( gram, proctext=TRUE )
{
  #Process text if proctext=TRUE (text is already processed for )
  
  w <- lastgrams(gram,proctext);
  
  res <- merge(otk_q[otk_q$tkey == paste(w[1],w[2],w[3]),],
               merge(otk_b[otk_b$ukey == w[3],],
                     otk_t[otk_t$bkey == paste(w[2],w[3]),],by=c("val"),
                     all=TRUE),by=c("val"),all=TRUE);
  oktu <- unique(rbind(otk_u[otk_u$val %in% res$val,],top_u));
  res <- merge(oktu, res ,by=c("val"),all=TRUE);
  
  res$pt[is.na(res$pt)] <- 0;
  res$pq[is.na(res$pq)] <- 0;
  res$pb[is.na(res$pb)] <- 0;
  res$pu[is.na(res$pu)] <- 0;
  
  res$Freq[is.na(res$Freq)] <- 0;
  res$bFreq[is.na(res$bFreq)] <- 0;
  res$tFreq[is.na(res$tFreq)] <- 0;
  res$Frequ[is.na(res$Frequ)] <- 0;
  
  #res$Ft <- res$Frequ + res$bFreq + res$tFreq + res$Freq; 
  #  res$p <- (res$pq * la3 * res$tFreq  + res$pt * la2 * res$bFreq + res$pb *  la1 * res$Freq *  +  la0 * res$pu * res$Frequ) / res$Ft;

  res$p <- (res$pq * la3   + res$pt * la2  + res$pb *  la1 +  la0 * res$pu );
  
  res <- res[order(res$p,decreasing = TRUE),];
  res <- res[res$val != "zznumzz",];
  
  outp <- NULL;
  outp$res <- res;
  outp$tokens <- w;
  
  return(outp);
}    


shinyServer(
  function(input, output) {
    
    output$tokens <-renderText({ 
      outp <<- fd_nextw(input$Sentence);  
                          paste(outp$tokens,collapse = " ", sep=" ");  
    });
    
    output$text1 <- renderText({ 
     input$Sentence;
      stri_join(outp$res$val[1],"    p= ",round(outp$res$p[1], digits=4));  
      
      });
    
    
    output$text2 <- renderText({ 
      input$Sentence; 
      stri_join(outp$res$val[2],"    p= ",round(outp$res$p[2], digits=4));  
      
    });

    output$text3 <- renderText({ 
      input$Sentence; 
      stri_join(outp$res$val[3],"    p= ",round(outp$res$p[3], digits=4));  
        
    });
    
      output$text4 <- renderText({ 
        input$Sentence; 
        stri_join(outp$res$val[4],"    p= ",round(outp$res$p[4], digits=4));  
        
      });
    
      output$text5 <- renderText({ 
        input$Sentence; 
        stri_join(outp$res$val[5],"    p= ",round(outp$res$p[5], digits=4));  
        
      });
    
    })