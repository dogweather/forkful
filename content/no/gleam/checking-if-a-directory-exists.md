---
title:                "Sjekke om en mappe eksisterer"
html_title:           "Gleam: Sjekke om en mappe eksisterer"
simple_title:         "Sjekke om en mappe eksisterer"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hvorfor?

Har du noen gang prøvd å åpne en fil eller lese en fil fra en mappe, bare for å finne ut at mappen ikke eksisterer? Det er ikke bare frustrerende, men det kan også føre til krasj i programmet ditt. Å sjekke om en mappe eksisterer før du prøver å gjøre noe med den, er en viktig del av å skrive pålitelig og feilfri kode.

## Hvordan du gjør det

For å sjekke om en mappe eksisterer i Gleam, bruker du funksjonen `std.fs.exists` og gir den mappen du vil sjekke som et argument. Denne funksjonen vil returnere en `Result`-type som enten er `Ok` hvis mappen eksisterer, eller `Err` hvis den ikke gjør det. Her er et eksempel på hvordan du kan bruke denne funksjonen:

```Gleam
// Sjekker om mappen "bilder" eksisterer
let resultat = std.fs.exists("bilder")

// Hvis resultatet er en "Ok"-verdi, skriver vi ut at mappen eksisterer
match resultat {
  Ok -> println("Mappen 'bilder' eksisterer!")
  Err -> println("Mappen 'bilder' eksisterer ikke.")
}
```

I dette eksempelet sjekker vi om mappen "bilder" eksisterer, og skriver ut en passende melding basert på resultatet. Du kan også bruke en `if`-setning for å håndtere resultatet:

```Gleam
if std.fs.exists("bilder") == Ok {
  println("Mappen 'bilder' eksisterer!")
} else {
  println("Mappen 'bilder' eksisterer ikke.")
}
```

## Dypere dykk

Som nevnt ovenfor, vil `std.fs.exists`-funksjonen returnere en `Result`-type. Dette er en type som kan være enten `Ok` eller `Err`, og den brukes mye i Gleam for å håndtere mulige feil. Hvis du vil ha en mer detaljert forklaring på hvordan `Result` fungerer, kan du sjekke ut dokumentasjonen [her](https://gleam.run/book/std/result.html).

Det er også verdt å merke seg at `std.fs.exists`-funksjonen kun sjekker om mappen eksisterer, den sjekker ikke om du har tilgang til den eller om det er andre problemer med mappen. Hvis du trenger mer avansert filbehandling, kan du se på `std.fs.open` og `std.fs.create`-funksjonene.

## Se også

- [Dokumentasjon for Gleam sin standardbibliotek](https://gleam.run/book/std/)
- [Gleam sin offisielle nettside](https://gleam.run/)
- [Kodeeksempler på GitHub](https://github.com/gleam-lang/gleam/tree/main/examples)