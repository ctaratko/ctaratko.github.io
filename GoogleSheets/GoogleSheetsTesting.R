library(googlesheets4)

googlesheets4::gs4_token()
googlesheets4::gs4_api_key()
# Gargle2.0
?googlesheets4::gs4_auth()
url = "https://docs.google.com/spreadsheets/d/1UC9KqQtgFholIHKr-_8fhwYOh2VxJkdWvN3IHge6Qjo/edit?pli=1&gid=0#gid=0"
Values <- c(1,2,3)
Sheet <- googlesheets4::read_sheet(ss = "WebsiteSpreadsheet")
Sheet <- Sheet %>% 
  sheet_append(Values)


?sheet_append()

globalenv()


