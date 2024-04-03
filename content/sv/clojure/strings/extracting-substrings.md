---
date: 2024-01-20 17:45:28.199557-07:00
description: "Hur man g\xF6r: Clojure erbjuder flera s\xE4tt att hantera delstr\xE4\
  ngar. Se exempel nedan."
lastmod: '2024-03-13T22:44:37.514354-06:00'
model: gpt-4-1106-preview
summary: "Clojure erbjuder flera s\xE4tt att hantera delstr\xE4ngar."
title: "Extrahera delstr\xE4ngar"
weight: 6
---

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
