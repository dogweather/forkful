---
title:                "Skrivande till standardfel"
html_title:           "Clojure: Skrivande till standardfel"
simple_title:         "Skrivande till standardfel"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Vad & Varför?

I Clojure, precis som i många andra programmeringsspråk, kan du skriva till standard error för att skicka felmeddelanden eller andra viktiga texter till en specifik utmatningskanal istället för den vanliga standardutmatningen. Detta kan hjälpa till att tydligt separera olika typer av utskrifter och underlätta felsökning vid behov.

Att skriva till standard error är också en bra praxis eftersom det inte blandas ihop med den vanliga standardutmatningen, vilket kan vara användbart när programmet körs i en konsol eller terminal.

## Hur man gör:

Clojure erbjuder flera möjligheter för att skriva till standard error. Den enklaste metoden är att använda funktionen `print-err` från standardbiblioteket `clojure.core`.

```Clojure
(print-err "Det här är ett felmeddelande på standard error.")
```

Om du vill formatera meddelandet eller inkludera flera värden, kan du använda `format`-funktionen istället.

```Clojure
(format-err "Det här är en formaterad text på standard error: %s" "1")
```

Båda dessa exempel kommer att skriva texten till standard error och ge följande utmatning i en repl-miljö:

```Clojure
Det här är ett felmeddelande på standard error.
Det här är en formaterad text på standard error: 1
```

## Deep Dive:

Skrivning till standard error uppstod som en standard inom Unix-operativsystemet på 1970-talet. Genom att skicka felmeddelanden till en separat utmatningskanal, separeras de från den vanliga användarutmatningen och kan lättare hittas och läsas av felsökningsteamet.

Alternativen till att skriva till standard error inkluderar att skicka meddelanden till en loggfil eller en externt hostad tjänst som kan hantera felmeddelanden. Men i många fall är det fortfarande användbart att använda standard error för omedelbara och viktiga utskrifter.

I Clojure implementeras standard error som en standard utmatningsström vid namn `*err*`, vilket kan ändras med hjälp av funktionen `binding`.

## Se även:

- Clojure Documentation om [standard-io](https://clojuredocs.org/clojure.core/*out*)
- Unix Manual Page för [sterr](https://man7.org/linux/man-pages/man3/stderr.3.html)