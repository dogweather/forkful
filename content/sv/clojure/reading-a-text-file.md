---
title:                "Clojure: Läsa en textfil"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Varför

Att läsa en textfil är en grundläggande operation inom programmering och en viktig färdighet att ha. Det kan hjälpa dig att hantera stora mängder data eller skapa program som interagerar med externa filer.

## Hur man gör det

För att läsa en textfil i Clojure, börja med att använda funktionen `slurp` för att läsa in hela filens innehåll som en sträng. Till exempel, om vi har en textfil som heter `namn.txt` med följande innehåll:

```
Anna
Erik
Maria
```

Då kan vi läsa in den med hjälp av följande kod:

```Clojure
(def names (slurp "namn.txt"))
```

Detta skapar en variabel som heter `names` som innehåller strängen "Anna\nErik\nMaria". Notera att `\n` representerar en radbrytning.

För att dela upp strängen i en vektor med namnen som element, använder vi funktionen `clojure.string/split` tillsammans med `\n` som separator. Vi kan också använda `clojure.string/trim` för att ta bort mellanslag och radbrytningar från början och slutet av varje element.

```Clojure
(require '[clojure.string :as str])

(def names-list (mapv str/trim (str/split names #"\n")))
```

Nu har vi en vektor med namnen `["Anna" "Erik" "Maria"]`. Vi kan sedan göra vad vi vill med denna vektor, som att skriva ut den, sortera den eller bearbeta den på något annat sätt.

## Djupdykning

För att hantera större textfiler kan vi använda funktionen `reader` tillsammans med `line-seq`. Detta hjälper oss att hantera inläsningen av filen mer effektivt, eftersom det inte behöver läsa in hela filen på en gång.

För att använda detta, först öppna filen med `reader` och sedan läsa varje rad med `line-seq`. För att läsa in data från en CSV-fil, använd följande kod:

```Clojure
(require '[clojure.data.csv :as csv])

(with-open [file-reader (reader "data.csv")]
  (doall (csv/read-csv (line-seq file-reader))))
```

Denna kod läser in varje rad i CSV-filen och returnerar det som en sekvens av vektorer som representerar varje rad. Mer information om hur man använder `clojure.data.csv` biblioteket finns här: [https://github.com/clojure/data.csv](https://github.com/clojure/data.csv)

## Se även

- [https://clojure.org/api/cheatsheet](https://clojure.org/api/cheatsheet) (en snabbreferens för Clojure-funktioner och API)
- [https://github.com/clojure-cookbook/](https://github.com/clojure-cookbook/) (en samling av vanliga programmeringsproblem och lösningar i Clojure)