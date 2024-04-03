---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:37.359114-07:00
description: "Hur man g\xF6r: #."
lastmod: '2024-03-13T22:44:37.543487-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: Att skriva en textfil
weight: 24
---

## Hur man gör:


### Skriva text till en fil med Clojures inbyggda funktioner
Funktionen `spit` är det enklaste sättet att skriva text till en fil i Clojure. Den tar två argument: filsökvägen och strängen som ska skrivas. Om filen inte finns kommer `spit` att skapa den. Om den gör det, kommer `spit` att skriva över den.

```clojure
(spit "exempel.txt" "Hej, världen!")
```

För att lägga till text i en befintlig fil kan du använda `spit`-funktionen med alternativet `:append`.

```clojure
(spit "exempel.txt" "\nLåt oss lägga till denna nya rad." :append true)
```

Efter att ha kört dessa kodsnuttar kommer "exempel.txt" att innehålla:

```
Hej, världen!
Låt oss lägga till denna nya rad.
```

### Använda tredjepartsbibliotek
Även om Clojures inbyggda möjligheter ofta är tillräckliga, har gemenskapen utvecklat robusta bibliotek för mer komplexa eller specifika uppgifter. För fil-I/O är ett populärt bibliotek `clojure.java.io`, som tillhandahåller ett mer Java-likt tillvägagångssätt för filhantering.

För att använda `clojure.java.io` för att skriva till en fil måste du först importera det:

```clojure
(require '[clojure.java.io :as io])
```

Därefter kan du använda funktionen `writer` för att få ett writer-objekt, och funktionen `spit` (eller andra som `print`, `println`) för att skriva till filen:

```clojure
(with-open [w (io/writer "exempel_med_io.txt")]
  (.write w "Detta är skrivet med hjälp av clojure.java.io"))
```

Detta kommer att skapa (eller skriva över om den redan finns) "exempel_med_io.txt" med texten:

```
Detta är skrivet med hjälp av clojure.java.io
```

Kom ihåg: `with-open` säkerställer att filen stängs korrekt efter skrivning, för att undvika potentiella resursläckor.
