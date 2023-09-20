library(rsconnect)
tmp.enc <- options()$encoding
options(encoding = "UTF-8")
rsconnect::deployApp("C:/Users/nadia/Desktop/Hector/desktop/FAO/costos-produccion")

rsconnect::setAccountInfo(name='hacamal',
                          token='AF9B253081454F21CA758E02ED2CF5DE',
                          secret='LleuUr+lLEGMsGUllrycPTauPd+F6jxq9dVpJGbc')
