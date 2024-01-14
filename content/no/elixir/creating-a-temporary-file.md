---
title:                "Elixir: Opprettelse av midlertidig fil"
simple_title:         "Opprettelse av midlertidig fil"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

##Hvorfor

Innenfor programmering er det ofte nødvendig å opprette midlertidige filer for å lagre data som ikke trengs permanent. Dette kan være nyttig for å lagre midlertidige informasjon som brukes i en prosess, eller for å lagre data som skal slettes senere. I Elixir, kan vi bruke funksjoner for å enkelt opprette midlertidige filer. I denne bloggposten skal vi se nærmere på hvorfor og hvordan man oppretter midlertidige filer i Elixir.

##Hvordan

For å opprette en midlertidig fil i Elixir, kan vi bruke `File.stream!/2`-funksjonen. Denne funksjonen tar imot to argumenter; navnet på den nye filen og moduset den skal opprettes i. Moduser kan være `:read`, `:write`, eller `:append` og bestemmer hvordan filen kan leses og skrives til. Her er et eksempel på hvordan man oppretter en midlertidig fil i Elixir:

```Elixir
{:ok, temp_file} = File.stream!("temp.txt", [:write])
IO.puts "Midlertidig fil opprettet: #{temp_file.path}"
```
Dette vil lage en midlertidig fil med navnet "temp.txt" i skrivemodus, og deretter skrive ut en bekreftelse på at filen ble opprettet sammen med filens plassering på datamaskinen.

For å skrive til filen, kan vi bruke `IO.write/2`-funksjonen. Som det første argumentet tar denne funksjonen filobjektet, og som det andre argumentet tar den teksten vi ønsker å skrive til filen. Her er et eksempel på hvordan man kan skrive teksten "Dette er en midlertidig fil" til den nye filen:

```Elixir
IO.write(temp_file, "Dette er en midlertidig fil")
```

Vi kan også lese data fra filen ved å bruke `IO.read/2`-funksjonen. Denne funksjonen tar filobjektet som det første argumentet og antall bytes som skal leses som det andre argumentet. Her er et eksempel på hvordan man kan lese de første 10 bytene fra filen:

```Elixir
IO.read(temp_file, 10) #=> "Dette er e"
```

Etter at vi har lagt til all den nødvendige informasjonen i den midlertidige filen, kan vi lukke den ved å bruke `File.close/2`-funksjonen. Dette vil slette den midlertidige filen automatisk og frigjøre eventuelle ressurser som ble brukt av den. Her er et eksempel på hvordan man lukker og sletter filen:

```Elixir
File.close(temp_file)
```

##Dykk ned i det

Nå som vi har sett på hvordan man oppretter, skriver til og leser fra midlertidige filer i Elixir, la oss se på noen viktige ting å huske på når man jobber med midlertidige filer:

- Midlertidige filer vil bli slettet automatisk når de blir lukket, så sørg for å ikke lagre viktig informasjon i dem.
- Pass på å bruke unike filnavn for å unngå konflikter med eksisterende filer.
- Hvis du jobber med følsomme data, bør du vurdere å slette den midlertidige filen manuelt etter bruk for å sikre at dataene ikke kan bli gjenopprettet.

##Se også

- [Elixir Dokumentasjon om midlertidige filer](https://hexdocs.pm/elixir/1.12.3/File.html#stream!/2)
- [Artikkel om håndtering av filer i Elixir](https://til.hashrocket.com/posts/ecw8gh3yyy-reading-writing-and-file-system-interactions-in-elixir)