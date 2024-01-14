---
title:    "Elixir: Å hente delstrenger"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

# Hvorfor

Elixir er et kraftig programmeringsspråk som gjør det enkelt å manipulere tekststrenger. En vanlig oppgave som ofte dukker opp er å trekke ut en del av en tekststreng, kalt en substrings.

## Hvordan

Elixir tilbyr forskjellige metoder for å ekstrahere substrings. En måte å gjøre det på er å bruke `String.slice/3`-funksjonen. Denne funksjonen tar inn en tekststreng, en startindeks og en lengde som argumenter. Her er et enkelt eksempel:

```Elixir
name = "Jeg elsker Elixir"
substring = String.slice(name, 3, 6)
IO.puts(substring)
```

Dette vil skrive ut "elsker" til konsollen. Vi gir `String.slice/3` funksjonen en startindeks på 3, og en lengde på 6, som betyr at vi vil ha en substring som starter på indeks 3 og inneholder 6 tegn.

Vi kan også bruke `String.slice/2`-funksjonen for å ekstrahere en substring basert på en gitt indeks og frem til slutten av tekststrengen:

```Elixir
name = "Jeg elsker Elixir"
substring = String.slice(name, 8)
IO.puts(substring)
```

Dette vil skrive ut "Elixir" siden vi gir funksjonen en startindeks på 8, som tilsvarer "E" i "Elixir".

## Dypdykk

Det finnes også andre metoder for å ekstrahere substrings i Elixir. `String.split/3` funksjonen kan brukes når vi vil dele en tekststreng basert på et gitt tegn, og returnere en liste med substrings. Her er et eksempel:

```Elixir
name = "Jeg elsker Elixir"
substring_list = String.split(name, " ")
IO.inspect(substring_list)
```

Dette vil returnere en liste med substrings basert på mellomrommet mellom hvert ord i den opprinnelige tekststrengen. Output vil være `["Jeg", "elsker", "Elixir"]`.

Et annet nyttig verktøy for å ekstrahere substrings er `String.contains?/2`-funksjonen. Denne kan brukes til å sjekke om en tekststreng inneholder en bestemt substring. Her er et eksempel:

```Elixir
name = "Jeg elsker Elixir"
IO.puts(String.contains?(name, "Elixir"))
```

Dette vil skrive ut `true` siden tekststrengen inneholder substringen "Elixir".

# Se også

- [Elixir docs: String module](https://hexdocs.pm/elixir/String.html)
- [Elixir School: String module](https://elixirschool.com/en/lessons/basics/string/)
- [Elixirforum: Substring extraction](https://elixirforum.com/t/substring-exact-matching-from-strings/13302)