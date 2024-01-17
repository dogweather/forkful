---
title:                "Att skriva en textfil"
html_title:           "Ruby: Att skriva en textfil"
simple_title:         "Att skriva en textfil"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skriva en textfil i programming innebär att skriva och spara en fil med textinformation på datorn. Det används vanligtvis av programmerare för att spara och återanvända till exempel kod eller data.

## Såhär gör du:
Att skriva en textfil i Ruby är enkelt och kan göras på olika sätt beroende på vad du vill uppnå. Här är två olika exempel på hur du kan göra.

```
# Exempel 1: Skriv en textfil med en enkel sträng
File.write("mittdokument.txt", "Det här är min första textfil i Ruby!")

# Exempel 2: Skriv en textfil med flera rader och variabler
# Ange filnamn och öppna filen i läge "w" för att skriva till filen
filnamn = "minadata.txt"
text = "Hej, jag heter Ruby.\nJag är ett programmeringsspråk.\nHär är lite information om mig:\nVersion: 2.7.4\nFörst utgivet: 21 december 1995"
File.open(filnamn, "w") do |fil|
  fil.write(text)
end
``` 
Output:
```
mittdokument.txt:
Det här är min första textfil i Ruby!

minadata.txt:
Hej, jag heter Ruby.
Jag är ett programmeringsspråk.
Här är lite information om mig:
Version: 2.7.4
Först utgivet: 21 december 1995
```

## Djupdykning:
Att skriva en textfil i Ruby är en del av filhanteringsfunktionerna som finns tillgängliga i språket. Det är ett enkelt och effektivt sätt att spara information på datorn. Det finns också andra sätt att hantera textfiler i Ruby, som att läsa in befintliga filer eller lägga till ny information i en befintlig fil. Det är också möjligt att använda metoder som ".read" eller ".each_line" för att läsa textfiler.

## Se även:
- Ruby dokumentation om filhantering: https://ruby-doc.org/core-3.0.2/File.html
- En guide om hur man hanterar textfiler i Ruby: https://www.sitepoint.com/using-ruby-file-operations/
- En tutorial för att läsa och skriva filer i Ruby: https://www.tutorialspoint.com/ruby/ruby_input_output.htm