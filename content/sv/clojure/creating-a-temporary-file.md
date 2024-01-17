---
title:                "Skapa en temporär fil"
html_title:           "Clojure: Skapa en temporär fil"
simple_title:         "Skapa en temporär fil"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

##Vad & Varför?
 Att skapa en temporär fil är en vanlig uppgift för programmerare när de behöver spara data temporärt under en pågående process. Det kan vara användbart för att undvika filkonflikter eller för att hantera data på ett mer säkert sätt.

##Så här:
```Clojure 
(def tmp-file (java.io.File/createTempFile "temp" ".txt"))
```

Ett exempel på att skapa en temporär fil i Clojure är genom att använda Java-funktionen ```createTempFile```, som tar in två parametrar för att specificera filnamnet och filtypen. Detta skapar en temporär fil som lagras på disk och returnerar en ```File``` objektreferens, som sedan kan användas för att hantera filen som vanligt.

```
#=> #object[java.io.File 0x660d974b "C:\Users\username\AppData\Local\Temp\temp5722457914398788016.txt"]
```

##Djupdykning:
Att skapa en temporär fil är en viktig del av många program och system, men det är inte något som alltid har funnits tillgängligt. Tidigare var det vanligt att program behövde skapa egna system för att hantera temporära filer, vilket kunde leda till problem med kompatibilitet och säkerhet.

Alternativ till att skapa en temporär fil kan vara att använda andra metoder för att hantera data, som till exempel att lagra informationen i minnet eller att använda en databas. Dock kan dessa alternativ inte alltid vara möjliga eller lämpliga, särskilt om data ska hanteras temporärt och behöver sparas på disk.

Implementationsdetaljer om att skapa en temporär fil kan variera beroende på vilket programmeringsspråk och ramverk som används. I Clojure är det vanligt att använda Java-funktioner för att hantera temporära filer, men det finns även Clojure-specifika bibliotek, som till exempel ```tempjure```, som erbjuder fler funktioner och alternativ för att skapa temporära filer.

##Se även:
- Java Dokumentation om ```File.createTempFile```: https://docs.oracle.com/javase/8/docs/api/java/io/File.html
- Tempjure biblioteket på Clojars: https://clojars.org/tempjure
- En diskussion om hantering av temporära filer i Clojure: https://stackoverflow.com/questions/2128100/creating-temporary-files-in-clojure