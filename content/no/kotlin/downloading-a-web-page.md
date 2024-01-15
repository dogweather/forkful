---
title:                "Å laste ned en nettside"
html_title:           "Kotlin: Å laste ned en nettside"
simple_title:         "Å laste ned en nettside"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor ville noen ønske å laste ned en nettside? Vel, det kan være mange grunner til det. Kanskje du vil lagre informasjonen for senere bruk, eller kanskje du vil ha en offline versjon av nettsiden. Uansett hva grunnen din er, er det enkelt å laste ned en nettside ved hjelp av Kotlin. 

## Hvordan gjøre det

Først må du importere nødvendige pakker. For å laste ned en nettside, trenger du å importere `java.net.URL` og `java.io.File`. Deretter kan du følge disse trinnene:

1. Lag en `URL`-instans med adressen til nettsiden du ønsker å laste ned, for eksempel: 
   
   ```Kotlin
   val url = URL("https://www.example.com")
   ```

2. Deretter må du opprette en `File`-instans som vil bli brukt til å lagre nettsiden. Du kan velge hvilken som helst filsti og filnavn du ønsker:
   
   ```Kotlin
   val fil = File("sti/til/fil/nettside.html")
   ```

3. Nå er det på tide å laste ned nettsiden! Du kan gjøre det ved å bruke `copyTo()`-funksjonen og angi `File`-instansen som mål:
   
   ```Kotlin
   url.openStream().copyTo(fil.outputStream())
   ```

4. Og det er alt! Nettsiden er nå lastet ned og lagret på ønsket filsti. Du kan nå åpne filen og se nettsiden offline når som helst.

## Dykk dypere

Det er verdt å merke seg at dette eksemplet bare laster ned selve HTML-siden til nettsiden, ikke hele nettsiden med bilder, CSS og JavaScript. For å laste ned alt dette innholdet, må du bruke en annen metode som involverer å analysere HTML-koden og finne lenker til alle bildene, CSS-filene og JavaScript-filene.

Det kan også være nyttig å legge til litt feilhåndtering i koden din. Dette kan gjøres ved å omgi koden din med et `try-catch`-blokk, og håndtere eventuelle unntak som oppstår under nedlastingsprosessen.

## Se også

- [Official Kotlin Documentation](https://kotlinlang.org/docs/reference/)
- [How to download a file in Kotlin](https://www.tutorialkart.com/kotlin/how-to-download-a-file-in-kotlin/)