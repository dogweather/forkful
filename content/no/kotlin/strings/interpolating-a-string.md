---
date: 2024-01-20 17:51:01.734972-07:00
description: "Strenginterpolasjon lar deg bake variabler og uttrykk rett inn i strenger,\
  \ noe som gj\xF8r koden mer lesbar og skrivbar. Programmerere bruker det for \xE5\
  \ sl\xE5\u2026"
lastmod: '2024-03-13T22:44:40.736399-06:00'
model: gpt-4-1106-preview
summary: "Strenginterpolasjon lar deg bake variabler og uttrykk rett inn i strenger,\
  \ noe som gj\xF8r koden mer lesbar og skrivbar. Programmerere bruker det for \xE5\
  \ sl\xE5\u2026"
title: Interpolering av en streng
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Strenginterpolasjon lar deg bake variabler og uttrykk rett inn i strenger, noe som gjør koden mer lesbar og skrivbar. Programmerere bruker det for å slå sammen data og tekst på en enkel og feilfri måte.

## Hvordan gjøre det:
```kotlin
fun main() {
    val brukernavn = "OlaNordmann"
    val poeng = 42
    println("Hei, $brukernavn! Du har akkurat oppnådd $poeng poeng!")
    println("Neste nivå er om ${(100 - poeng)} poeng.")
}
```
Utskrift:
```
Hei, OlaNordmann! Du har akkurat oppnådd 42 poeng!
Neste nivå er om 58 poeng.
```

## Dykk ned i det:
Strenginterpolasjon i Kotlin er inspirert av lignende funksjonalitet i andre moderne språk som Ruby og JavaScript. Denne funksjonen ble introdusert for å forenkle prosessen med å bygge strenger. 

Alternativer til strenginterpolasjon inkluderer den gamle metoden med å bruke `+` for å sammenslå verdier og strenger eller bruk av `String.format()`. 

Interpolasjonen evaluerer uttrykket innenfor `${}` og konverterer det til en streng. Hvis det bare er en variabel, kan du droppe krøllparentesene.

## Se Også:
- Kotlin dokumentasjon om strengmaler: [Kotlin String Templates](https://kotlinlang.org/docs/basic-syntax.html#string-templates)
- Oracle's Java tutorials om `String.format()`: [Java String Format](https://docs.oracle.com/javase/tutorial/java/data/strings.html)
