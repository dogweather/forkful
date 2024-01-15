---
title:                "Skriva en textfil"
html_title:           "Clojure: Skriva en textfil"
simple_title:         "Skriva en textfil"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Varför
Att skriva en textfil är en grundläggande färdighet som är oumbärlig för programmerare. Det ger en strukturerad och läsbar form för kod, vilket gör det lättare att felsöka och samarbeta med andra.

## Så här gör du
Att skriva en textfil i Clojure är enkelt. Du kan använda kommandot `spara` för att skapa en ny textfil med valfritt namn och filändelse. Sedan kan du öppna filen och börja skriva din kod med hjälp av Clojure syntax. Till exempel:

```Clojure
(spara "min_kod.clj")
(+ 1 2 3) ;; Det här är ett exempel på kod
```

När du är klar med din kod kan du köra filen genom att skriva `lein exec` följt av din filnamn. Detta kommer att exekvera koden och visa resultatet.

## Djupdykning
En textfil i Clojure består av en samling av kodblokk som är avgränsade av parenteser. Kodblokkarna kan innehålla en eller flera s-formulär som utgör själva koden. Varje kodblokk måste börja med en öppnande parentes `(` och avslutas med en stängande parentes `)`. Till exempel:

```Clojure
(+ 1 2) ;; detta är ett kodblokk
```

I Clojure finns också möjligheten att kommentera din kod med hjälp av semikolon `;`. Allt som kommer efter denna kommentarsymbol anses vara en kommentar och ignoreras av Clojure.

## Se även
- Clojure officiella hemsida: https://clojure.org/
- Officiell dokumentation för Clojure: https://clojure.org/documentation
- En handledning för att komma igång med Clojure: https://github.com/pgundlur/clojure-basics