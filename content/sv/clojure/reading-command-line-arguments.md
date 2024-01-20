---
title:                "Läsa kommandoradsargument"
html_title:           "Bash: Läsa kommandoradsargument"
simple_title:         "Läsa kommandoradsargument"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Vad & Varför?

I programmering, man läser kommandoradsargument för att kontrollera programmet baserat på dessa värden. Dessa argument ger input direkt vid start och möjliggör anpassning utan att ändra koden.

## Så här gör du:

Här är ett enkelt exemplet på att läsa kommandoradsargument i Clojure.

```Clojure
(defn -main 
  [& args]
  (println args))
```
Om du kör exemplet ovan med argumentet `hello`, kommer den att skriva ut `hello`.

## Djup dykning

Historiskt sett, läsning av kommandoradsargument har varit en kärnkomponent i många programmeringsspråk sedan början av programmering. Alternativa metoder inkluderar att använda inmatning under exekvering eller att läsa från en fil, men kommandoradsargument ger större flexibilitet.

När det gäller implementeringsdetaljer, `& args` i `-main` funktionen samlar alla argument som skickas till den i en lista. Clojure använder sig av JVM under huven, och det är JVM som initialt tar emot argumenten och sedan passar dem till Clojure-programmet.

## Se också

1. Clojure Documentation: [Command Line Arguments](https://clojure.org/guides/repl/command_line_arguments)
2. Blog Post: [Command Line Arguments in Clojure](https://www.baeldung.com/clojure-command-line-arguments)
3. Stack Overflow: [How to pass command line arguments in Clojure](https://stackoverflow.com/questions/4301629/how-to-pass-command-line-arguments-to-a-clojure-program)