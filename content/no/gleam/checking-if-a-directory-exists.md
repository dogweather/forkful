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

# Hva & Hvorfor?
Sjekker du om en mappe eksisterer er en viktig del av programmering. Dette gjøres for å sikre at koden fungerer som den skal, og for å unngå feil og problemer med dataen som brukes.

# Slik gjør du det:
I Gleam kan du enkelt sjekke om en mappe eksisterer ved å bruke ```std.fs.exists```. Dette vil returnere en boolsk verdi som viser om mappen eksisterer eller ikke. Du kan også bruke ```std.fs.is_dir``` for å spesifikt sjekke om en mappe eksisterer.

Eksempel:
```
// Sjekker om mappen "dokumenter" eksisterer
let eksisterer = std.fs.exists("dokumenter")
// Vil returnere true eller false, avhengig av om mappen eksisterer eller ikke
```

Eksempel på hvordan du kan bruke en betinget uttalelse for å håndtere situasjonen der mappen ikke eksisterer:
```
if std.fs.exists("bilder") {
    // Gjør noe med dataen i mappen
} else {
    // Håndter tilfellet der mappen ikke eksisterer
}
```

# Ta et dypdykk:
Å sjekke om en mappe eksisterer er en vanlig praksis i programmering, spesielt når du jobber med filbehandling. Dette har vært en del av programmering lenge før Gleam ble utviklet, og er fortsatt like viktig som før.

Alternativer til å bruke ```std.fs.exists``` inkluderer å bruke andre språk som støtter filbehandling, som for eksempel Rust eller Go. I tillegg finnes det også tredjepartsbiblioteker som tilbyr lignende funksjonalitet.

Implementeringen av ```std.fs.exists``` er basert på operativsystemets funksjonalitet for å sjekke om en fil eller mappe eksisterer. Det er derfor viktig å være klar over eventuelle begrensninger og ulikheter mellom forskjellige operativsystemer når du bruker denne funksjonen.

# Se også:
- [Gleam sin offisielle dokumentasjon om std.fs.exists](https://gleam.run/documentation/stdlib/fs.html#exists)