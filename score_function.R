score <- function(resultado){
  resultado = (table(resultado))
  no_cereza = resultado
  no_cereza["CZ"] = 0
  switch (resultado["CZ"],
          1 = return(1), 
          2 = return(3),
          3 = return(5)
  )
  if (as.vector(no_cereza)[1]==3){
    switch (no_cereza[1],
      L = return(10), 
      U = return(15),
      N = return(20),
      S = return(25),
      BN = return(30),
      CP = return(35),
      7 = return(50),
      BAR = return(100)
    )
  }else{
    return(0)
  }
}
