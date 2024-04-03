---
date: 2024-01-26 03:39:05.947606-07:00
description: "Hur man g\xF6r: I Clojure \xE4r str\xE4ngar of\xF6r\xE4nderliga, s\xE5\
  \ n\xE4r vi pratar om att \"ta bort citattecken\" pratar vi egentligen om att skapa\
  \ en ny str\xE4ng utan\u2026"
lastmod: '2024-03-13T22:44:37.513426-06:00'
model: gpt-4-0125-preview
summary: "I Clojure \xE4r str\xE4ngar of\xF6r\xE4nderliga, s\xE5 n\xE4r vi pratar\
  \ om att \"ta bort citattecken\" pratar vi egentligen om att skapa en ny str\xE4\
  ng utan citattecken."
title: "Ta bort citattecken fr\xE5n en str\xE4ng"
weight: 9
---

## Hur man gör:
I Clojure är strängar oföränderliga, så när vi pratar om att "ta bort citattecken" pratar vi egentligen om att skapa en ny sträng utan citattecken. Här är det enkla sättet med `clojure.string/replace`:

```clojure
(require '[clojure.string :as str])

; Låt oss bli av med dessa dubbla citattecken
(defn remove-double-quotes [s]
  (str/replace s #"\"" ""))

; Och sparka ut de enkla citattecken
(defn remove-single-quotes [s]
  (str/replace s #"\'" ""))

; Exempelanvändning:
(remove-double-quotes "\"Hej, världen!\"") ; => "Hej, världen!"
(remove-single-quotes "'Hej, världen!'")   ; => "Hej, världen!"
```
Vill du hantera både enkla och dubbla citattecken i ett svep? Kolla här:

```clojure
(defn remove-quotes [s]
  (str/replace s #"[\"\']" ""))

; Exempelanvändning:
(remove-quotes "\"Hej, 'Clojure' världen!\"") ; => "Hej, Clojure världen!"
```

## Fördjupning
Förr i tiden när data var rörigare än ett barns sovrum, var citattecken i strängar normen för att beteckna text. Men när datavetenskapen utvecklades, blev citattecken mer än bara textavgränsare - de tog på sig syntaktiska roller i programmeringsspråk.

Clojure, med sitt Lisp-arv, använder inte citattecken på samma sätt som vissa andra språk kanske gör. De används definitivt för att beteckna strängar, men de har även en särskild roll i att skapa literaler. Oavsett så förblir uppgiften att ta bort citattecken från strängar en tidlös uppgift.

Varför inte bara skära bort ändarna på en sträng? Tja, det är att anta att dina citattecken alltid kramar början och slutet av din sträng som ett par överdrivet kärleksfulla morföräldrar. Verkligheten är rörigare. Ange regex (reguljära uttryck), vilket låter dig rikta in dig på dessa citattecken oavsett var de gömmer sig.

Alternativ? Visst, du kan bli finurlig med `subs`, `trim`, `triml`, `trimr`, eller till och med transducers om du vill visa upp dig. Men `replace` med regex är som att ta med ett lasersvärd till en knivfight - det skär rakt på sak.

## Se också
Om din hjärna kliar efter mer Clojure-strängmanipulationers under, kan dessa brödsmulor hjälpa:

- ClojureDocs om `clojure.string/replace`: https://clojuredocs.org/clojure.string/replace
- Reguljära uttryck i Clojure: https://clojure.org/guides/learn/syntax#_regex
- Java-interoperabilitet för stränghantering (Clojure körs på JVM trots allt): https://clojure.org/reference/java_interop#_working_with_strings

Stanna inte vid att ta bort citattecken. Det finns en hel värld av strängmagi där ute i Clojure-land, som väntar på att upptäckas.
