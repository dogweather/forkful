---
title:                "Sökning och ersättning av text"
html_title:           "Arduino: Sökning och ersättning av text"
simple_title:         "Sökning och ersättning av text"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Sökning och ersättning av text är ett sätt att hitta specifika tecken eller strängar i en mängd text och byta ut dem mot något annat. Programmerare gör detta för att ändra variabelnamn, korrigera felstavningar, uppdatera kod och mer.

## Hur man gör det:

Här är hur du gör en sökning och ersättning av text i Clojure:

```Clojure
(defn replace-text [text old new]
  (clojure.string/replace text old new))
```
Låt oss prova den:

```Clojure
(replace-text "Hej värld" "värld" "Clojure")
```
Utgången kommer att bli:

```Clojure
"Hej Clojure"
```
På så sätt skapar vi en funktion för att byta ut textsträngar.

## Djupdykning

- **Historiskt sammanhang**: Sökning och ersättning är en grundläggande funktion i många programmeringsspråk, inklusive de äldre som Lisp, som Clojure är en modern dialekt av. Den har varit en kärnfunktion i textredigerare och IDE:er sedan dess tidiga dagar.

- **Alternativ**: Om du vill byta ut flera olika strängar kan du använda `clojure.string/replace` med en map som argument. Till exempel skulle `(clojure.string/replace "Hej värld" { "värld" "Clojure", "Hej" "Hello" })` returnera "Hello Clojure".

- **Implementationdetaljer**: Funktionen `clojure.string/replace` kan ta tre former: två strängar, en sträng och en map, eller en sträng och en reguljär uttryck. Det hanterar dygnet-runt för att hitta och ersätta alla förekomster på det mest effektiva sättet.

## Se även

För mer information om att manipulera strängar i Clojure, se följande resurser:

- Clojure - String Functions: https://clojuredocs.org/clojure.string
- Introduction to Strings in Clojure: https://purelyfunctional.tv/guide/introduction-to-strings-in-clojure/

För att fördjupa dig i Clojure och dess användning inom textmanipulering, är dessa böcker utmärkta guider:

- "Clojure for the Brave and True" av Daniel Higginbotham
- "Getting Clojure" av Russ Olsen