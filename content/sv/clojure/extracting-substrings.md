---
title:                "Extrahera delsträngar"
date:                  2024-01-20T17:45:28.199557-07:00
model:                 gpt-4-1106-preview
simple_title:         "Extrahera delsträngar"

category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att extrahera delsträngar handlar om att plocka specifika bitar av text ut från en större sträng. Programmerare gör detta för att manipulera, analysera eller transformera data i mer användbara format.

## Hur man gör:
Clojure erbjuder flera sätt att hantera delsträngar. Se exempel nedan:

```Clojure
;; Använd `subs` för att extrahera en delsträng från position 0 till 5.
(defn extract-substr []
  (subs "Hejsan Sverige!" 0 5))

(extract-substr)
;; Output: "Hejsan"

;; Använd `clojure.string/includes?` för att kontrollera om en delsträng finns.
(defn contains-substr? [str substr]
  (clojure.string/includes? str substr))

(contains-substr? "Hejsan Sverige!" "Sverige")
;; Output: true
```

## Fördjupning
Att extrahera delsträngar har funnits så länge som programmeringsspråk har jobbat med text. Det har blivit lite av en grundsten i textbearbetning. I Clojure, där strängar är immutable, garanterar funktioner som `subs` att originalsträngen förblir oförändrad. Alternativ inkluderar att använda regex med `re-find` eller `re-matches` för mer komplex substängsmanipulation.

I termer av implementation använder Clojure Javas underliggande stränghantering för att genomföra operationer, vilket betyder att det är både snabbt och tillförlitligt. 

## Se även
- "Clojure for the Brave and True" om strängbearbetning: [https://www.braveclojure.com/clojure-for-the-brave-and-true/](https://www.braveclojure.com/clojure-for-the-brave-and-true/)
- ClojureDocs, en community-drivna exempelsamling: [https://clojuredocs.org/](https://clojuredocs.org/)
