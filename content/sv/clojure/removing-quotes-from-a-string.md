---
title:                "Ta bort citattecken från en sträng"
date:                  2024-01-26T03:39:05.947606-07:00
model:                 gpt-4-0125-preview
simple_title:         "Ta bort citattecken från en sträng"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Vad och varför?
Att ta bort citattecken från en sträng innebär att bli av med de där irriterande dubbla eller enkla citattecken som omsluter din text. Programmerare gör detta för att rensa data, säkerställa enhetlighet eller förbereda strängar för bearbetning där citattecken är oönskade eller kan orsaka fel.

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