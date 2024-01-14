---
title:                "Clojure: Stora bokstäver i en sträng"
simple_title:         "Stora bokstäver i en sträng"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att automatiskt härleda en sträng är en vanlig programmeringsuppgift. Det kan vara användbart för att skapa läsbara dokumenttitlar, formattera text i enlighet med stil, eller helt enkelt för att förbättra användarupplevelsen.

## Hur man gör

```Clojure
; Här är ett exempel som illustrerar hur man kan avsluta en sträng
(defn capital-case [str]
  (->> str
       ; omvandlar strängen till en sekvens av ord
       (clojure.string/split #" ")
       ; Capitalize första bokstaven på varje ord
       (map #(str/capitalize %))
       ; Gå ihop alla ord igen
       (clojure.string/join " ")))

(def example-string "det här är en exempelsträng")

(capital-case example-string)
; Output: "Det Här Är En Exempelsträng"
```

## Djupdykning

Förutom att bara kalla på `capitalize` funktionen, kan det också vara användbart att känna till de olika parametrarna som kan användas för att anpassa hur strängen härleds.

### `clojure.string/capitalize`

Denna funktion hanterar specialtecken, vilket innebär att den inte kommer att ändra en sträng som redan är helt store bokstäver.

### `clojure.string/upper-case`

Denna funktion kommer att konvertera alla tecken till store bokstäver oavsett hur de var i början.

### `clojure.string/lower-case`

Som du kanske gissat kommer denna funktion att konvertera alla tecken till små bokstäver.

## Se även

- [Clojure string hantering](https://clojure.org/about/strings)
- [Clojure string funktioner](https://clojuredocs.org/clojure.string)
- [Capitalize en sträng i Clojure](https://stackabuse.com/capitalize-a-string-in-clojure/)