---
title:    "Elixir: Leser en tekstfil"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Hvorfor

Å kunne lese tekstfiler er en viktig ferdighet for enhver programmerer. Det lar deg enkelt få tilgang til og bearbeide store mengder data, noe som er avgjørende for å lage effektive og pålitelige programmer.

## Slik gjør du det

Elixir har en innebygd funksjon, `File.read!`, som lar deg lese en tekstfil og returnere innholdet som en streng. La oss se på et eksempel:

```Elixir
content = File.read!("tekstfil.txt")
IO.puts(content)
```

Her leser vi innholdet i filen "tekstfil.txt" og skriver det ut til konsollen ved hjelp av `IO.puts`-funksjonen. Hvis filen ikke eksisterer eller det oppstår en feil, vil `File.read!` kaste en feilmelding.

Hvis du heller vil returnere innholdet som en liste med linjer, kan du bruke `File.read_lines!` i stedet:

```Elixir
lines = File.read_lines!("tekstfil.txt")
Enum.each(lines, fn line -> IO.puts(line) end)
```

Her bruker vi `Enum.each`-funksjonen til å iterere gjennom hver linje i filen og skrive den ut.

## Dypdykk

Hvis du ønsker å lese og bearbeide tekstfiler på en mer avansert måte, kan du bruke `File.stream!`. Denne funksjonen åpner en strøm (stream) til filen, noe som betyr at innholdet blir lest inn chunk for chunk i stedet for å bli lastet inn i minnet på én gang. Dette er spesielt nyttig hvis du skal lese veldig store filer som kan føre til at programmet ditt går tom for minne.

For å ta en titt på denne metoden, kan du se på følgende eksempel:

```Elixir
File.stream!("tekstfil.txt") |> Stream.each(&IO.puts/1) |> Stream.run
```

Her åpner vi en strøm til filen "tekstfil.txt" og bruker `Stream.each`-funksjonen til å skrive ut hver linje til konsollen. Deretter kaller vi `Stream.run` for å faktisk kjøre strømmen.

## Se også

- [Elixir Dokumentasjon om Fil IO](https://hexdocs.pm/elixir/File.html)
- [Elixir Dokumentasjon om Streams](https://hexdocs.pm/elixir/Stream.html)
- [Elixir Dokumentasjon om Enum](https://hexdocs.pm/elixir/Enum.html)