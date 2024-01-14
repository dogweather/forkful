---
title:    "Elixir: Å sjekke om en mappe eksisterer"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Hvorfor

Hvis du noen gang har skrevet et program som trenger å håndtere filer og mapper, har du kanskje kommet over situasjoner der du må sjekke om en mappe eksisterer. Ved å sjekke dette kan du unngå feil og sikre at programmene dine kjører jevnt. I denne blogginnlegget vil vi se på hvordan du kan sjekke om en mappe eksisterer ved hjelp av Elixir.

# Hvordan

For å sjekke om en mappe eksisterer i Elixir, kan vi bruke funksjonen `File.dir?`. Denne funksjonen tar inn en sti som argument og returnerer `true` hvis den finner en mappe på stien eller `false` hvis den ikke gjør det. La oss se et eksempel på hvordan vi kan bruke denne funksjonen:

```Elixir
if File.dir?("path/to/directory") do
  IO.puts("Mappen finnes!")
else
  IO.puts("Mappen finnes ikke.")
end
```

I dette eksempelet sjekker vi om mappen `path/to/directory` eksisterer. Hvis den gjør det, skriver vi ut en melding som sier at mappen finnes. Hvis den ikke eksisterer, skriver vi ut en melding som sier at mappen ikke finnes.

# Dypdykk

Nå som vi har sett hvordan vi kan bruke `File.dir?` for å sjekke om en mappe eksisterer, la oss dykke litt dypere inn i denne funksjonen. En ting du bør være klar over er at denne funksjonen sjekker for både mapper og symbolic links. Hvis du vil ekskludere symbolic links fra sjekken, kan du bruke funksjonen `File.dir_entries` i stedet.

Det er også verdt å merke seg at `File.dir?` vil også returnere `false` hvis stien du gir inn peker til en fil i stedet for en mappe. Hvis du vil sjekke om en fil eksisterer, kan du bruke funksjonen `File.file?`.

# Se også

- [Elixir Dokumentasjon: File.dir?/1](https://hexdocs.pm/elixir/File.html#dir?/1)
- [Elixir Dokumentasjon: File.file?/1](https://hexdocs.pm/elixir/File.html#file?/1)
- [Elixir Dokumentasjon: File.dir_entries/1](https://hexdocs.pm/elixir/File.html#dir_entries/1)