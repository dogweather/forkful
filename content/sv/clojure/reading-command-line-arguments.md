---
title:                "Läsning av kommandoradsargument"
html_title:           "Clojure: Läsning av kommandoradsargument"
simple_title:         "Läsning av kommandoradsargument"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Varför
Att läsa kommandoradargument kan vara användbart för att få program att interagera med användaren och ta emot input från dem utan att behöva skriva in hela programmet varje gång. Det kan också vara användbart för att anpassa beteendet hos ett program beroende på användarens preferenser.

## Hur man gör
Det finns ett enkelt sätt att läsa kommandoradargument i Clojure. Vi använder funktionen "command-line-args" för att få en lista över alla argument som matas in när programmet körs.

```Clojure
(def args (command-line-args))
```

Om jag kör följande kommando från terminalen:
```bash
$ clojure myprogram.clj arg1 arg2
```

Då kommer "args" att vara en lista med strängarna "arg1" och "arg2".

Vi kan sedan använda detta för att få programmet att agera på ett visst sätt beroende på vilka argument som matas in. Till exempel kan vi ha en liten funktion som hälsar på användaren baserat på vad de matar in som sitt namn:

```Clojure
(defn greeting [name]
    (println "Hej" name ", välkommen till vårt program!"))

(def args (command-line-args))

(if (= (count args) 2)
    (greeting (last args))
    (println "Förlåt, jag behöver ditt namn också!"))
```

Om jag kör detta program med följande kommando:
```bash
$ clojure hello.clj Mikael
```
Då kommer följande output att visas:
```
Hej Mikael, välkommen till vårt program!
```
Medan om jag bara skriver:
```bash
$ clojure hello.clj
```
Då kommer vi att få följande output:
```
Förlåt, jag behöver ditt namn också!
```

## Djupdykning
Det finns flera funktioner i Clojure som kan användas för att hantera kommandoradargument, såsom "slurp" och "split". Det finns också bibliotek som "clj-cli" som gör det möjligt att definiera kommandoradsargument på ett mer formellt sätt.

För att få en bättre förståelse för hur kommandoradsargument hanteras i Clojure, kan det vara bra att läsa på om hur strängar, listor och vektorer fungerar i språket.

## Se även
- [Clojure's command-line-args function](https://clojuredocs.org/clojure.core/command-line-args)
- [clj-cli library](https://github.com/clojure/tools.cli)
- [Split and slurp functions in Clojure](https://clojuredocs.org/clojure.core/split,https://clojuredocs.org/clojure.core/slurp)