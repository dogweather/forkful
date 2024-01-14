---
title:                "Clojure: Läsning av en textfil"
simple_title:         "Läsning av en textfil"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Varför

Att läsa textfiler är en grundläggande färdighet inom programmering och kan vara användbart för att hantera eller manipulera stora mängder data. I denna bloggpost kommer vi att ta en titt på hur man läser en textfil i Clojure och hur man kan använda detta för att lösa olika programmeringsproblem.

## Hur man gör det

För att läsa en textfil i Clojure använder vi funktionen `slurp`, som läser in hela filinnehållet som en sträng. Vi kan sedan använda olika inbyggda funktioner för att bearbeta denna sträng, till exempel `split`, `filter`, `map` och `reduce`. Låt oss ta en titt på ett enkelt exempel på hur man läser en fil och räknar antalet ord i den:

```Clojure
(def filinnehall (slurp "testfil.txt"))
(def ord (split filinnehall #" "))
(def antal-ord (count ord))
(println "Antal ord i filen: " antal-ord)
```

Om vår textfil `testfil.txt` innehåller "Hej, jag heter Lisa och jag älskar att programmera.", kommer konsoloutputen att vara:

```
Antal ord i filen: 10
```

## Djupdykning

Det finns många användbara funktioner i Clojure för att läsa och manipulera textfiler. En annan användbar funktion är `line-seq`, som läser en fil rad för rad och returnerar dessa som en sekvens. Detta kan vara användbart när man arbetar med stora textfiler och inte vill läsa in allt på en gång. En annan funktion är `spit`, som skriver en sträng till en fil.

Det är också värt att notera att Clojure har inbyggd stöd för första-klassens funktioner, vilket innebär att vi kan skicka funktioner som argument till andra funktioner. Detta kan vara användbart vid bearbetning av filinnehåll, till exempel om vi vill filtrera ut vissa rader eller mappa över varje rad med en viss funktion.

## Se även

- [Clojure dokumentation - Input and Output](https://clojure.org/guides/io)
- [Clojure Cookbook - Working with Files](https://clojure-cookbook.com/division-14.html)
- [Clojure för programmerare - Läs från och skriv till filer](https://clojure-for-rubyists.readthedocs.io/en/latest/file_io.html)