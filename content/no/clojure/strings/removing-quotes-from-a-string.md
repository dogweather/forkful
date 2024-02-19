---
aliases:
- /no/clojure/removing-quotes-from-a-string/
date: 2024-01-26 03:38:45.831893-07:00
description: "\xC5 fjerne anf\xF8rselstegn fra en streng betyr \xE5 kvitte seg med\
  \ de irriterende doble eller enkle anf\xF8rselstegnene som omslutter teksten din.\
  \ Programmerere\u2026"
lastmod: 2024-02-18 23:08:53.553255
model: gpt-4-0125-preview
summary: "\xC5 fjerne anf\xF8rselstegn fra en streng betyr \xE5 kvitte seg med de\
  \ irriterende doble eller enkle anf\xF8rselstegnene som omslutter teksten din. Programmerere\u2026"
title: "Fjerne anf\xF8rselstegn fra en streng"
---

{{< edit_this_page >}}

## Hva og hvorfor?
Å fjerne anførselstegn fra en streng betyr å kvitte seg med de irriterende doble eller enkle anførselstegnene som omslutter teksten din. Programmerere gjør dette for å rense data, sikre uniformitet, eller forberede strenger for behandling der anførselstegn er uønskede eller kan forårsake feil.

## Hvordan:
I Clojure er strenger uforanderlige, så når vi snakker om "å fjerne anførselstegn," snakker vi egentlig om å skape en ny streng uten anførselstegn. Her er det slanke med `clojure.string/replace`:

```clojure
(require '[clojure.string :as str])

; La oss kvitte oss med disse doble anførselstegnene
(defn remove-double-quotes [s]
  (str/replace s #"\"" ""))

; Og kaste ut enkle anførselstegn
(defn remove-single-quotes [s]
  (str/replace s #"\'" ""))

; Eksempelbruk:
(remove-double-quotes "\"Hello, World!\"") ; => "Hello, World!"
(remove-single-quotes "'Hello, World!'")   ; => "Hello, World!"
```
Vil du håndtere både enkle og doble anførselstegn i ett fell swoop? Ta en titt på dette:

```clojure
(defn remove-quotes [s]
  (str/replace s #"[\"\']" ""))

; Eksempelbruk:
(remove-quotes "\"Hello, 'Clojure' World!\"") ; => "Hello, Clojure World!"
```

## Dypdykk
Tilbake i dagene da data var rotete som et barnerom, var anførselstegn i strenger normen for å angi tekst. Men ettersom datavitenskapen utviklet seg, ble anførselstegn mer enn bare tekstavgrensere – de tok på seg syntaktiske roller i programmeringsspråk.

Clojure, med sin Lisp-arv, bruker ikke anførselstegn på samme måte som noen andre språk kan gjøre. De brukes sikkert til å betegne strenger, men de har også en spesiell rolle i å skape literaler. Uansett forblir fjerning av anførselstegn fra strenger en tidløs oppgave.

Hvorfor ikke bare skjære av endene av en streng? Vel, det antar at dine anførselstegn alltid klemmer starten og slutten av strengen din som et par over-kjærlige besteforeldre. Virkelige data er rotete. Skriv inn regex (regulære uttrykk), som lar deg målrette disse anførselstegnene uansett hvor de gjemmer seg.

Alternativer? Klart, du kan bli fancy med `subs`, `trim`, `triml`, `trimr`, eller til og med transdusere hvis du vil vise frem. Men `replace` med regex er som å bringe en lyssabel til en knivkamp – det skjærer rett til jakten.

## Se også
Hvis hjernen din klør etter mer Clojure-strengmanipuleringsgodhet, kan disse smuler hjelpe:

- ClojureDocs om `clojure.string/replace`: https://clojuredocs.org/clojure.string/replace
- Regulære uttrykk i Clojure: https://clojure.org/guides/learn/syntax#_regex
- Java interop for strengbehandling (Clojure kjører på JVM tross alt): https://clojure.org/reference/java_interop#_working_with_strings

Ikke stopp med å fjerne anførselstegn. Det er en hel verden av strengmagi der ute i Clojure-landet som venter på å bli oppdaget.
