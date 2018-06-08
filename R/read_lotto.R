address <- "http://www.nlotto.co.kr/gameResult.do?method=allWinExel&gubun=byWin&nowPage=&drwNoStart=1&drwNoEnd=9999"

download.file(address, destfile = "test.xls")

tables <- XML::readHTMLTable("test.xls")

history_lotto <- tables[[2]]
