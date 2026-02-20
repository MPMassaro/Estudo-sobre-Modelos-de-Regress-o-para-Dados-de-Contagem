plot.alt = function(fit,mfrow=c(2,2)){
  resid = resid(fit)
  
  oldpar = par(no.readonly = TRUE)
  on.exit(par(oldpar))
  
  par(mfrow=mfrow)
  
  # 1) Residuos vs Valores Ajustados
  plot(fitted(fit), resid,
       main="Resíduos vs Valores Ajustados",
       ylab = "Resíduos Quantílicos",
       xlab = "Valores Ajustados",
       pch = 16,
       cex.lab = 1.1,
       cex.main=1.25,
       ylim = c(min(resid, -2.5),max(resid, 2.5)))
  abline(h = c(-2, 2), lty = 2, col = "red")
  abline(h = 0, lty = 2, col = "blue")
  
  # 2) Resíduos vs Índice
  plot(resid,
       main="Resíduos vs Índice",
       xlab = "Índice",
       ylab = "Resíduos Quantílicos",
       pch = 16,
       cex.lab = 1.1,
       cex.main=1.25,
       ylim = c(min(resid, -2.5),max(resid, 2.5)))
  abline(h = c(-2, 2), lty = 2, col = "red")
  abline(h = 0, lty = 2, col = "blue")
  
  # 3) Densidade Estimada
  plot(density(resid),
       main = "Densidade Estimada",
       ylab = "Densidade",
       xlab = "Resíduos Quantílicos",
       cex.lab = 1.1,
       cex.main=1.25)
  rug(resid, col = "red")
  
  # 4) QQ-plot
  qqnorm(resid,
         pch = 16,
         main = "QQ-plot dos Resíduos Quantílicos",
         ylab = "Quantis Observados",
         xlab = "Quantis Teóricos",
         cex.lab = 1.1,
         cex.main=1.25,
         xlim = c(min(resid-0.25), max(resid+0.25)),
         ylim = c(min(resid-0.25), max(resid+0.25)))
  abline(0, 1, col = "red")

}
