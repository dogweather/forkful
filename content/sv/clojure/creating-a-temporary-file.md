---
title:                "Clojure: Skapa en temporär fil"
simple_title:         "Skapa en temporär fil"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Varför
Att skapa en tillfällig fil är en användbar funktion i Clojure för att temporärt lagra data eller resultat från en process. Det kan vara särskilt användbart när man arbetar med större datamängder eller i situationer där man inte vill permanent spara data.

## Hur man gör
För att skapa en tillfällig fil i Clojure kan man använda sig av den inbyggda funktionen "with-tempfile". Den tar två parametrar, en prefix för filnamnet och ett suffix som anger filtypen. Här är ett exempel på hur man kan skapa en tillfällig fil och skriva lite data till den:

```Clojure
(with-tempfile "mittprefix" ".txt"
  (spit "Det här är mitt tillfälliga filinnehåll" %))
```

I detta exempel kommer en fil med namnet "mittprefix[random-nummer].txt" att skapas och innehålla texten "Det här är mitt tillfälliga filinnehåll". Det tillfälliga filobjektet returneras av funktionen och kan sedan användas för att läsa och bearbeta filen vidare.

## Deep Dive
Förutom att använda "with-tempfile" finns det även andra bibliotek i Clojure som erbjuder mer avancerade funktioner för att skapa tillfälliga filer. Ett exempel är "java.io.File/createTempFile", som ger möjlighet att ställa in parametrar som filens plats, namn och filtyp.

När man skapar en tillfällig fil är det också viktigt att tänka på säkerheten. Det finns alltid en risk att temporära filer kan läsas eller ändras av obehöriga användare. För att undvika detta kan man använda sig av säkrare sätt att skapa och hantera filer, som till exempel "java.nio.file.Files/createTempFile".

# Se även
- [Clojuredocs - with-tempfile](https://clojuredocs.org/clojure.core/with-tempfile)
- [JavaDocs - java.io.File/createTempFile](https://docs.oracle.com/javase/8/docs/api/java/io/File.html#createTempFile-java.lang.String-java.lang.String-java.io.File-)
- [JavaDocs - java.nio.file.Files/createTempFile](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html#createTempFile-java.lang.String-java.lang.String-java.nio.file.attribute.FileAttribute...-)