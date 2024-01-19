---
title:                "Opprette en midlertidig fil"
html_title:           "C#: Opprette en midlertidig fil"
simple_title:         "Opprette en midlertidig fil"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å lage en midlertidig fil er en prosess hvor vi oppretter en fil som bare skal være tilgjengelig for en kort periode. Programmerere gjør dette for å lagre data som trengs i løpet av en kjøretid, men som ikke behøver å være en del av sluttresultatet eller vedvarende data.

## Hvordan:

Bruke `File` modulen i Elixir for å lage midlertidige filer. Se eksempel under:

```Elixir
{:ok, io_device} = File.open("tmp.txt", [:write])
IO.binwrite(io_device, "Midlertidig data")
File.close(io_device)
```

Dette lager en midlertidig fil som heter "tmp.txt" og skriver "Midlertidig data" i filen. 

## Dypdykk

Midlertidige filer har lenge vært en betrodd funksjon i mange programmeringsspråk. I unix-baserte systemer, der Elixir stammer fra, er `/tmp` mappen ofte brukt til lagring av slike filer.

Alternativt, kan du bruke funksjonen `System.tmp_dir/0` i Elixir for å finne ut hvor operativsystemet foretrekker å plassere midlertidige filer.

En viktig ting å merke seg er at disse filene skal slettes når de ikke lenger er nødvendig. Elixir tilbyr `File.rm/1` funksjon for å slette en fil:

```Elixir
:ok = File.rm("tmp.txt")
```

## Se også:

For mer informasjon om filhåndtering i Elixir, se følgende ressurser:

- Elixir offisiell dokumentasjon på File modulen: [https://hexdocs.pm/elixir/File.html](https://hexdocs.pm/elixir/File.html)
- Diskusjon om midlertidige filer i Elixir i Elixir Forum: [https://elixirforum.com/t/how-to-best-work-with-temporary-files/1833](https://elixirforum.com/t/how-to-best-work-with-temporary-files/1833)
- Erlang/OTP's fil modul dokumentasjon som Elixir's er bygd på: [http://erlang.org/doc/man/file.html](http://erlang.org/doc/man/file.html)