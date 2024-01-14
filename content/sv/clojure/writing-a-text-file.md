---
title:                "Clojure: Skriva en textfil"
simple_title:         "Skriva en textfil"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Varför

Att skriva en textfil kan vara en användbar färdighet för alla som vill lära sig programmera och skapa datorprogram. Det ger dig möjlighet att skriva och lagra information på ett enkelt sätt som kan läsas av både människor och datorer.

## Hur man gör det

För att skriva en textfil i Clojure behöver du bara en textredigerare och tillgång till Clojure-överflödet. Börja med att öppna en textredigerare som till exempel Notepad eller Emacs. Skriv sedan din kod i redigeraren enligt Clojure-syntaxen. När du är nöjd med din kod kan du spara filen med ett namn och lägga till filändelsen .clj så att den kan läsas av Clojure-överflödet.

```Clojure
(print "Hej världen!")
```

Detta exempel skriver ut texten "Hej världen!" när du kör det i Clojure-överflödet. Ett annat nyttigt verktyg är "println" som automatiskt lägger till en radbrytning efter texten.

```Clojure
(println "Välkommen till min blogg!")
```

Detta exempel skriver ut texten "Välkommen till min blogg!" på en egen rad.

## Djupdykning

När du skriver en textfil i Clojure kan du använda en rad olika funktioner och datastrukturer. En vanlig sådan struktur är listor, som representeras med parenteser och innehåller olika värden eller funktioner. Ett annat exempel är map, där du kan associera nycklar med värden.

```Clojure
(def person {:namn "Anna" :ålder 30 :titel "Programmerare"})
```

Detta exempel skapar en map med information om en person. Du kan få åtkomst till enskilda värden genom att använda en punkt och nyckelns namn, till exempel "person.namn" för att få ut namnet "Anna".

## Se även

- [Clojure Dokumentation](https://clojure.org/guides/learn/syntax)
- [Clojure Överflöde](https://clojure.org/guides/learn/functions)
- [Textredigerare för Clojure](https://clojure.org/guides/getting_started#_text_editors)