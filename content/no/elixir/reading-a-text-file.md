---
title:                "Lese en tekstfil"
html_title:           "C#: Lese en tekstfil"
simple_title:         "Lese en tekstfil"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?

Å lese en tekstfil handler om å hente inn data fra en lagret fil. Programmerere gjør dette for å få tilgang til, analysere og manipulere dataen som er lagret i disse filene. Dette er spesielt viktig når det kommer til databasehåndtering og informasjonsprosessering.

## Hvordan:

Her er et enkelt eksempel på hvordan å lese en tekstfil i Elixir:

```Elixir
File.read("tekstfil.txt")
```

Kjører du koden over vil du se noe slikt som output:

```Elixir
{:ok, "Dette er en prøvetekst i en tekstfil."}
```

For å håndtere en potensiell feilsituasjon, kan vi bruke en match-operasjon sånn:

```Elixir
{:ok, innhold} = File.read("tekstfil.txt")
IO.puts(innhold)
```

## Dypdykk:

Å lese tekstfiler er en gammel praksis i programmering som går helt tilbake til tidlig dager av datamaskiner. I Elixir, en funksjonell programmeringsspråk bygget på Erlang virtual maskin, håndteres I/O operasjoner med innebygde moduler som `File` og `IO`.

Det er flere måter å håndtere leseoperasjoner på i Elixir. En alternativ måte er å bruke `File.stream!` funksjonen. Denne funksjonen lar deg behandle en fil som en strøm av linjer, noe som kan være praktisk for store filer.

Diskusjonen rundt implementeringsdetaljer er ganske teknisk, men det er viktig å nevne at Elixir håndterer I/O attraktivt ved å bruke Erlangs styrke i lyset av skalerbarhet og feiltoleranse. 

## Se også:

- Ønsker du å lese mer om filhåndtering i Elixir, sjekk ut [den offisielle dokumentasjonen](https://hexdocs.pm/elixir/File.html)
  
- For mer informasjon om `File.stream!` og strømmer generelt i Elixir, les [Streams-guiden](https://hexdocs.pm/elixir/Stream.html)

- En dyptgående diskusjon av I/O i Erlang og Elixir kan bli funnet i denne [posten på Erlang Solutions blogg](https://www.erlang-solutions.com/blog/erlang-and-elixir-i-o-its-not-what-you-think.html).