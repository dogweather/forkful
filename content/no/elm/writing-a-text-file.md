---
title:                "Elm: Å skrive en tekstdokument"
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor skulle noen engasjere seg i å skrive en tekstfil? Vel, en tekstfil er et viktig verktøy i programmering, spesielt i språket Elm. Det lar deg lagre og organisere data på en måte som er enkel å bruke og forstå.

## Hvordan

For å skrive en tekstfil i Elm, må du følge disse trinnene:

```
Elm.Text.join "\n" ["Hei!", "Dette er en tekstfil.", "Du kan skrive så mye du vil her."]

```

Dette vil skrive en tekstfil med tre linjer, der hver linje er en streng. Når du kjører koden, vil du få dette resultatet i tekstfilen:

```
Hei!
Dette er en tekstfil.
Du kan skrive så mye du vil her.
```

Som du kan se, er det veldig enkelt å lage en tekstfil i Elm. Du kan også legge til flere linjer og manipulere teksten på forskjellige måter, avhengig av dine behov og ønsker.

## Deep Dive

Nå som du vet hvordan du kan skrive en tekstfil i Elm, la oss gå litt dypere inn i emnet. En viktig ting å merke seg er at når du skriver en tekstfil, må du spesifisere filnavnet og hvor du vil at filen skal lagres. Dette gjøres ved å bruke funksjonen `File.write`, som ser slik ut:

```
File.write "minTekstfil.txt" "Dette er en tekstfil som inneholder noe viktig informasjon." 
```

I dette eksempelet vil koden opprette en tekstfil med navnet "minTekstfil.txt" og legge til teksten du har spesifisert som innholdet i filen. Du kan også spesifisere en sti til hvor du ønsker å lagre filen, for eksempel `File.write "/min/mappe/tekstfil.txt"`.

Det er også viktig å merke seg at hvis du ønsker å legge til mer tekst i en tekstfil som allerede eksisterer, må du bruke funksjonen `File.append` i stedet for `File.write`. Dette vil legge til ny tekst i slutten av den eksisterende filen, i stedet for å overskrive alt som allerede er skrevet.

## Se også

- Offisiell dokumentasjon for filmanipulasjon i Elm: https://guide.elm-lang.org/effects/file_system.html
- Introduction to Elm Programming: https://www.freecodecamp.org/news/a-beginners-introduction-to-elm-programming/
- Building a simple todo list app in Elm: https://medium.com/@joomiguelcunha/building-a-simple-todo-app-with-elm-54e0a1ac2378