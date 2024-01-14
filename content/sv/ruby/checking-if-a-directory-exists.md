---
title:    "Ruby: Kontrollera om en mapp finns"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför

Många gånger när vi programmerar i Ruby, kan vi behöva kontrollera om en viss mapp finns. Det kan vara för att avgöra om en fil kan sparas, hämtas eller för att hantera undantag. Oavsett anledning, är det viktigt att förstå hur man på ett enkelt sätt kan kontrollera om en viss mapp finns. I denna bloggpost kommer vi att utforska detta ämne och lära oss olika sätt att kontrollera mappens existens i Ruby.

## Hur man gör det

För att kontrollera om en mapp finns i Ruby, finns det två huvudsakliga metoder som vi kan använda: `Dir.exist?` och `File.directory?`. Båda dessa metoder returnerar en boolesk true/false beroende på om mappen finns eller inte.

```Ruby
# Kontrollera om mapp finns med hjälp av Dir.exist?
Dir.exist?('mappens_namn') #=> true/false

# Kontrollera om mapp finns med hjälp av File.directory?
File.directory?('mappens_namn') #=> true/false
```

Som ni kan se är syntaxen för båda metoderna ganska enkel. Det enda som behövs är namnet på mappen som vi vill kontrollera. Om mappen existerar kommer output att vara `true` och om den inte existerar kommer output att vara `false`.

## Djupdykning

För att förstå hur `Dir.exist?` och `File.directory?` fungerar, måste vi först förstå hur en mapp struktureras på datorn. En mapp kan innehålla flera olika filer och undermappar. När vi använder de två metoderna ovan, kontrollerar vi bara om själva mappen existerar och inte dess innehåll. Om vi vill kontrollera om en specifik fil eller undermapp finns i en mapp, måste vi använda `Dir.glob` tillsammans med en wildcard som följande:

```Ruby
# Kontrollera om filen "text.txt" finns i mappen "mappens_namn"
Dir.glob("mappens_namn/text.txt") #=> ["mappens_namn/text.txt"]

# Kontrollera om undermappen "bilder" finns i mappen "mappens_namn"
Dir.glob("mappens_namn/bilder") #=> ["mappens_namn/bilder/"]
```

Detta kan hjälpa oss att kontrollera en specifik fil eller undermapp inuti en mapp. Slutförandet av wildcard, "*" i slutet av sökvägen, ger oss allt innehåll i den specifika mappen.

## Se också 

-[`Dir.exist?` Dokumentation](https://ruby-doc.org/core-2.7.0/Dir.html#method-c-exists-3F) 
- [`File.directory?` Dokumentation](https://ruby-doc.org/core-2.7.0/File.html#method-c-directory-3F)
- [`Dir.glob` Dokumentation](https://ruby-doc.org/core-2.7.0/Dir.html#method-c-glob)