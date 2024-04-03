---
date: 2024-01-20 17:34:25.627131-07:00
description: "I programmering er det \xE5 sette sammen strenger, kalt konkatenering,\
  \ rett og slett \xE5 smelte sammen tekster. Vi gj\xF8r det for \xE5 bygge opp dynamiske\u2026"
lastmod: '2024-03-13T22:44:40.394580-06:00'
model: gpt-4-1106-preview
summary: "I programmering er det \xE5 sette sammen strenger, kalt konkatenering, rett\
  \ og slett \xE5 smelte sammen tekster."
title: "Sammensl\xE5ing av strenger"
weight: 3
---

## Hva & Hvorfor?
I programmering er det å sette sammen strenger, kalt konkatenering, rett og slett å smelte sammen tekster. Vi gjør det for å bygge opp dynamiske meldinger, lage filstier, eller behandle brukerinput.

## Hvordan:
```Clojure
;; Ved bruk av str funksjonen
(str "Hei, " "verden!")
;; => "Hei, verden!"

;; Ved bruk av clojure.core/strcat
(clojure.string/strcat "Clojure " "er " "kult!")
;; => "Clojure er kult!"

;; Sammenslåing av en samling med join
(clojure.string/join ["Clojure" "på" "enkelt"])
;; => "Clojurepåenkelt"

;; Med skille
(clojure.string/join " " ["Funksjonell" "programmering" "ftw!"])
;; => "Funksjonell programmering ftw!"
```

## Dypdykk
I de tidlige dagene av programmering var hukommelsen og prosessorkraften så begrenset at hver byte og syklus talte, noe som gjorde konkatenering kostbart. I Clojure, en moderne Lisp-dialekt, håndteres strengkonkatenering elegant og uten så mye merkbar belastning på systemet.

Alternativer til direkte konkatenering inneholder blant annet bruk av `format` funksjonen for å sette sammen tekster med variabler, eller å bygge opp strenger ved hjelp av `StringBuilder` i Java, som Clojure er bygget på toppen av.

Når vi konkatenerer strenger i Clojure, gjøres dette ofte gjennom `str`-funksjonen som tar en vilkårlig mengde argumenter og retunerer dem som en enkelt streng. `clojure.string/strcat` er en annen funksjon som har samme formål men er mindre brukt. `join` fra `clojure.string` biblioteket er nyttig når du har en samling av elementer som du vil kombinere til en string med eller uten et skille.

## Se Også
- ClojureDocs for mer om [str](https://clojuredocs.org/clojure.core/str)
- En grundig guide om strings i Clojure på [Clojure for the Brave and True](https://www.braveclojure.com/clojure-for-the-brave-and-true/)
- Informasjon om strengbehandling og ytelse på [Clojure's Java Interop](https://clojure.org/reference/java_interop) 페이지
