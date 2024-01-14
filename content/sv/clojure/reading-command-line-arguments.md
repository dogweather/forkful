---
title:    "Clojure: Att läsa kommandoradsargument"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Varför

Att läsa kommandoradsargument är en viktig färdighet för alla som programmerar med Clojure (eller något annat programmeringsspråk). Det låter dig interagera med ditt program på ett mer direkt sätt och kan ge användbar information vid felsökning eller testning.

## Hur man gör

För att läsa kommandoradsargument i Clojure använder vi funktionen `command-line-args`, som tar emot ingång från kommandoraden och returnerar en vektor av strängar. Låt oss titta på ett exempel:

```Clojure
(def args (command-line-args))
(println "Det första argumentet är:" (nth args 0))
```

Om vi kör detta program med kommandot `clj minprogram.clj hej` kommer det första argumentet som skrivs ut att vara `hej`. Vi kan också komma åt andra och efterföljande argument genom att ändra indexet i `nth`-funktionen. Om vi till exempel ändrar det till `1` kommer det andra argumentet att skrivas ut.

## Djupdykning

En viktig sak att notera är att `command-line-args` returnerar alla argument som strängar, även om de kan representera andra datatyper. Om vi till exempel skulle köra kommandot `clj minprogram.clj 5` kommer värdet av `args` att vara vektorn `["5"]`, inte talet 5 som du kanske förväntar dig.

En annan viktig aspekt att tänka på är att ordningen på argumenten spelar roll. Om vi skulle ändra kommandot till `clj minprogram.clj hej 5` skulle det första argumentet fortfarande vara `hej`, eftersom det är det första argumentet i kommandot.

## Se även

- [Clojure Dokumentation om command-line-args](https://clojuredocs.org/clojure.core/command-line-args)
- [Artikel om kommandoradsargument i Clojure](https://martiancraft.com/blog/2019/02/clj-args/)
- [En annan informativ guide till kommandoradsargument i Clojure](https://www.baeldung.com/clojure-command-line-arguments)