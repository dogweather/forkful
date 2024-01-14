---
title:                "Elixir: Å sjekke om en mappe eksisterer"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Hvorfor

Noen ganger i programmeringsverdenen, spesielt når man jobber med filbehandling, trenger man å sjekke om en bestemt mappe eksisterer før man utfører ytterligere operasjoner. Dette kan være for å unngå unødvendige feilmeldinger eller sikre at de riktige filene er tilgjengelige før de brukes. I denne bloggposten vil vi se nærmere på hvordan man kan sjekke om en mappe eksisterer ved hjelp av Elixir-programmeringsspråket.

# Hvordan gjøre det

For å sjekke om en mappe eksisterer i Elixir, kan vi bruke funksjonen `File.exists?/1` fra `File`-modulen. Dette vil returnere en boolsk verdi som indikerer om mappen eksisterer eller ikke. La oss se på et eksempel:

```Elixir
folder_path = "min-mappe/"
if File.exists?(folder_path) do
  IO.puts "Mappen #{folder_path} eksisterer."
else
  IO.puts "Mappen #{folder_path} eksisterer ikke."
end
```

I dette eksempelet sjekker vi om mappen "min-mappe" eksisterer. Hvis den gjør det, vil vi få utskriften "Mappen min-mappe/ eksisterer.", ellers vil vi få utskriften "Mappen min-mappe/ eksisterer ikke.".

# Dypdykk

Hvis du ønsker å sjekke om en mappe eksisterer på en spesifikk plassering, kan du bruke funksjonen `File.read_dir/1` fra `File`-modulen. Denne funksjonen vil returnere en liste med alle filer og mapper som finnes på den gitte plasseringen. Hvis mappen du leter etter er en del av denne listen, betyr det at den eksisterer. La oss se på et eksempel:

```Elixir
folder_path = "/bruker/navn/dokumenter/"
file_list = File.read_dir(folder_path)
if Enum.member?(file_list, "min-mappe") do
  IO.puts "Mappen #{folder_path} eksisterer."
else
  IO.puts "Mappen #{folder_path} eksisterer ikke."
end
```

I dette eksempelet sjekker vi om mappen "min-mappe" ligger i mappen "dokumenter" på brukerens plassering. Hvis den gjør det, vil vi få utskriften "Mappen /bruker/navn/dokumenter/ eksisterer.", ellers vil vi få utskriften "Mappen /bruker/navn/dokumenter/ eksisterer ikke.".

# Se også

- [Elixir File-modulen dokumentasjon](https://hexdocs.pm/elixir/File.html)
- [Elixir Enum-modulen dokumentasjon](https://hexdocs.pm/elixir/Enum.html)