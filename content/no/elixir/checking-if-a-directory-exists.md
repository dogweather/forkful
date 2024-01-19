---
title:                "Sjekker om en katalog eksisterer"
html_title:           "Elixir: Sjekker om en katalog eksisterer"
simple_title:         "Sjekker om en katalog eksisterer"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Elixir Programmering: Sjekker om en Mappe Eksisterer

## Hva & Hvorfor?
I Elixir, informerer "å sjekke om en mappe eksisterer" oss om hvorvidt en bestemt mappe er til stede på et gitt sted i systemet. Dette er nødvendig blant annet for å unngå fil- og mappe-relaterte feil i programmer.

## Hvordan til:
Her er hvordan du kan sjekke om en mappe eksisterer med Elixir:

```Elixir
:file.dir?("your/directory/path")
```

Hvis mappen eksisterer, vil funksjonen returnere `:ok`. Hvis ikke, vil den returnere `:error`. Her er et eksempel på bruk:

```Elixir
case :file.dir?("/tmp") do
  :ok ->
    IO.puts("Mappe eksisterer.")
  :error ->
    IO.puts("Mappe eksisterer ikke.")
end
```

Kjør dette programmet, og utdataen vil skrive ut om mappen "/tmp" eksisterer eller ikke.

## Dykket dypere
Historisk sett har denne funksjonaliteten vært et nøkkelaspekt av mange programmeringsspråk, inkludert Elixir, på grunn av dens integrasjon med operativsystemet. I Elixir er metoden for dette laget i Erlang-behendigheter, som det er tilfellet med mye av filsystemfunksjonaliteten.

Alternativene inkluderer å bruke `File.ls/1` for å liste opp kataloginnhold og deretter se om mappens navn er til stede. Dette er dog mindre effektivt, da det krever mer systemressurser.

Når du kaller `:file.dir?/1`, opprettholder Elixir en kobling til underliggende operativsystemtjenester for å utføre sjekken, ved bruk av Erlang/OTP's `:file`-modul.

## Se Også
1. Elixir's Dokumentasjon om `:file.dir?/1`: https://hexdocs.pm/elixir/1.12/File.html#module-checking-existence
2. Erlang's `:file` modul dokumentasjon: https://erlang.org/doc/man/file.html