---
title:    "Elixir: Sjekke om en mappe eksisterer"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Hvorfor
Det er mange grunner til å sjekke om en mappe eksisterer i Elixir-programmering. Noen ganger må du kanskje sjekke om en spesifikk mappe finnes før du oppretter en ny, eller du kanskje prøver å navigere til en eksisterende mappe. Uansett årsak, er det viktig å kunne sjekke om en mappe eksisterer for å unngå feil i koden din.

## Hvordan
I Elixir kan du enkelt sjekke om en mappe eksisterer ved å bruke `File.exists?` funksjonen. Her er et eksempel på hvordan du kan gjøre det i koden din:

```elixir
if File.exists?("mappe_navn") do
  IO.puts("Mappen finnes!")
else
  IO.puts("Mappen finnes ikke!")
end
```

Hvis mappen `mappe_navn` eksisterer i din nåværende arbeidsmappe, vil utskriften være "Mappen finnes!", ellers vil den være "Mappen finnes ikke!".

Du kan også bruke `File.dir?("mappe_navn")` funksjonen til å sjekke om en mappe er en faktisk mappe eller bare en fil. Dette kan være nyttig hvis du ønsker å behandle mapper og filer på forskjellige måter. Her er et eksempel på hvordan du kan bruke denne funksjonen:

```elixir
if File.dir?("mappe_navn") do
  IO.puts("Dette er en mappe!")
else
  IO.puts("Dette er ikke en mappe!")
end
```

Hvis `mappe_navn` er en mappe, vil utskriften være "Dette er en mappe!", ellers vil den være "Dette er ikke en mappe!".

## Dypdykk
Under overflaten bruker Elixir `File.exists?` og `File.dir?` funksjonene egentlig `:file.exists?` og `:file.dir?` som blir kalt i Erlang-VM (virtual machine). Disse funksjonene sjekker om en fil eller mappe eksisterer på operativsystemnivå. Hvis du vil lære mer om hvordan disse funksjonene fungerer, kan du lese dokumentasjonen for `:file` modulen i Erlang.

En annen ting å merke seg er at disse funksjonene returnerer `true` eller `false`, i motsetning til å kaste en unntak hvis mappen ikke eksisterer. Derfor kan du annen måte sjekke om en mappe eksisterer, men denne metoden er anbefalt for enkelhet og lesbarhet i koden din.

## Se også
* [Elixir File modul dokumentasjon](https://hexdocs.pm/elixir/File.html)
* [Erlang :file modul dokumentasjon](http://erlang.org/doc/man/file.html)
* [Elixir Dir modul dokumentasjon](https://hexdocs.pm/elixir/Dir.html)