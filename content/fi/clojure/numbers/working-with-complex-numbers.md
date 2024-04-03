---
date: 2024-01-26 04:39:00.704047-07:00
description: "Kuinka: Clojure tarjoaa sis\xE4\xE4nrakennetun tuen kompleksiluvuille\
  \ `clojure.lang.Numbers` apuluokan kautta. K\xE4yt\xE4 `complex`-toimintoa kompleksilukujen\u2026"
lastmod: '2024-03-13T22:44:56.178505-06:00'
model: gpt-4-0125-preview
summary: "Clojure tarjoaa sis\xE4\xE4nrakennetun tuen kompleksiluvuille `clojure.lang.Numbers`\
  \ apuluokan kautta."
title: "Kompleksilukujen k\xE4sittely"
weight: 14
---

## Kuinka:
Clojure tarjoaa sisäänrakennetun tuen kompleksiluvuille `clojure.lang.Numbers` apuluokan kautta. Käytä `complex`-toimintoa kompleksilukujen luomiseen ja aritmeettisten laskutoimitusten suorittamiseen.

```clojure
;; Kompleksilukujen luominen
(def a (clojure.lang.Numbers/complex 3 4))  ; 3 + 4i
(def b (clojure.lang.Numbers/complex 1 -1)) ; 1 - i

;; Yhteenlasku
(+ a b) ;=> #object[clojure.lang.Numbers.Complex 0x5c6cfe9 "4 + 3i"]

;; Vähennyslasku
(- a b) ;=> #object[clojure.lang.Numbers.Complex 0x5e51118 "2 + 5i"]

;; Kertolasku
(* a b) ;=> #object[clojure.lang.Numbers.Complex 0x6ec3f0df "7 + i"]

;; Jakolasku
(/ a b) ;=> #object[clojure.lang.Numbers.Complex 0x5db0cd10 "3.5 + 3.5i"]

;; Konjugaatti
(.conjugate a) ;=> #object[clojure.lang.Numbers.Complex 0x47c6e076 "3 - 4i"]
```

## Syväsukellus
Matemaatikot, kuten Gauss ja Euler, formalisoivat kompleksiluvut 1700-luvulla. Vaikka ne alkuun kohtasivat skeptisyyttä, ne ovat sittemmin tulleet olennaiseksi osaksi modernia tiedettä ja tekniikkaa. Clojuressa ei ole natiivia kompleksilukutyyppiä kuten joissakin kielissä (esim. Python), mutta mukana oleva Java-yhteistyö voi käsitellä tarvittavat operaatiot `clojure.lang.Numbers`-luokan kautta.

Javan `java.lang.Complex` on vankka vaihtoehto, joka tarjoaa enemmän ominaisuuksia ja potentiaalisia optimointeja. Clojuren isäntäyhteistyömahdollisuus helpottaa työskentelyä Java-kirjastojen kanssa.

Taustalla kompleksilukujen aritmeettinen käsittely käsittää reaaliosien ja kuvitteellisten osien lisäämisen ja kertomisen, avainregulaarina on, että `i^2 = -1`. Kompleksilukujen jakaminen voi olla mutkikkaampaa, tyypillisesti vaatien konjugaatin välttämiseksi jakamista kompleksiluvuilla.

## Katso Myös
- ClojureDocs, pikaviitteeksi: https://clojuredocs.org/
- Java API `java.lang.Complex` varten: https://docs.oracle.com/javase/8/docs/api/java/lang/Complex.html
- Wikipedia-sivu kompleksiluvuista matemaattisesti uteliaille: https://en.wikipedia.org/wiki/Complex_number
