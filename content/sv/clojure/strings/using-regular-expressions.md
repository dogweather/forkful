---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:35.949534-07:00
description: "Regulj\xE4ra uttryck, ett kraftfullt verktyg f\xF6r m\xF6nsterigenk\xE4\
  nning och datahantering, \xE4r essentiella i textbehandlingsuppgifter s\xE5som att\
  \ validera\u2026"
lastmod: '2024-03-13T22:44:37.515310-06:00'
model: gpt-4-0125-preview
summary: "Regulj\xE4ra uttryck, ett kraftfullt verktyg f\xF6r m\xF6nsterigenk\xE4\
  nning och datahantering, \xE4r essentiella i textbehandlingsuppgifter s\xE5som att\
  \ validera\u2026"
title: "Att anv\xE4nda regulj\xE4ra uttryck"
weight: 11
---

## Vad & Varför?
Reguljära uttryck, ett kraftfullt verktyg för mönsterigenkänning och datahantering, är essentiella i textbehandlingsuppgifter såsom att validera inmatning, söka och ersätta text. Programmerare använder dem omfattande för att hantera komplex strängtolkning och datavalidering effektivt och koncist.

## Hur:
Clojure, som håller sig trogen till sina rötter i Lisp-familjen, erbjuder ett rikt utbud av funktioner som integrerar sömlöst med Javas möjligheter för reguljära uttryck. Så här kan du utnyttja dem:

### Grundläggande matchning
För att kontrollera om en sträng matchar ett mönster, använd `re-matches`. Den returnerar hela matchningen om den lyckas eller `nil` annars.

```clojure
(re-matches #"\d+" "123")  ;=> "123"
(re-matches #"\d+" "abc")  ;=> nil
```

### Söka efter mönster
För att hitta den första förekomsten av ett mönster är `re-find` din funktion att gå till:

```clojure
(re-find #"\d+" "Order 123")  ;=> "123"
```

### Fånga grupper
Använd `re-find` tillsammans med parenteser i ditt mönster för att fånga grupper:

```clojure
(let [[_ area code] (re-find #"(1)?(\d{3})" "Telefon: 123-4567")]
  (println "Riktnummer:" area "Kod:" code))
;; Utdata: Riktnummer: nil Kod: 123
```

### Global sökning (Hitta alla träffar)
Clojure har inte en inbyggd global sökning som vissa språk. Använd istället `re-seq` för att få en lös följd av alla träffar:

```clojure
(re-seq #"\d+" "id: 123, antal: 456")  ;=> ("123" "456")
```

### Dela upp strängar
För att dela upp en sträng baserat på ett mönster, använd `clojure.string/split`:

```clojure
(clojure.string/split "John,Doe,30" #",")  ;=> ["John" "Doe" "30"]
```

### Ersätta
Ersätt delar av en sträng som matchar ett mönster med `clojure.string/replace`:

```clojure
(clojure.string/replace "2023-04-01" #"\d{4}" "ÅÅÅÅ")  ;=> "ÅÅÅÅ-04-01"
```

### Tredjepartbibliotek
Även om Clojures inbyggda stöd räcker för de flesta fall, för mer komplexa scenarier, överväg att använda bibliotek som `clojure.spec` för robust datavalidering och `reagent` för reaktiv DOM-manipulation i webbapplikationer med regex-baserad routing och validering av inmatning.

```clojure
;; Exempel med användning av clojure.spec för att validera en e-postadress
(require '[clojure.spec.alpha :as s])
(s/def ::email (s/and string? #(re-matches #".+@.+\..+" %)))
(s/valid? ::email "test@example.com")  ;=> sant
```

Kom ihåg, även om reguljära uttryck är kraftfulla, kan de också göra koden svårläst och svår att underhålla. Använd dem med omdöme och överväg alltid enklare strängmanipuleringsfunktioner där det är möjligt.
