---
date: 2024-01-20 17:53:55.205184-07:00
description: "\xC5 lese en tekstfil handler om \xE5 laste innholdet fra filen inn\
  \ i programmet. Programmerere gj\xF8r dette for \xE5 manipulere data, lagre innstillinger,\
  \ eller\u2026"
lastmod: '2024-03-11T00:14:13.988533-06:00'
model: gpt-4-1106-preview
summary: "\xC5 lese en tekstfil handler om \xE5 laste innholdet fra filen inn i programmet.\
  \ Programmerere gj\xF8r dette for \xE5 manipulere data, lagre innstillinger, eller\u2026"
title: Lese en tekstfil
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å lese en tekstfil handler om å laste innholdet fra filen inn i programmet. Programmerere gjør dette for å manipulere data, lagre innstillinger, eller lese kommandoer.

## Hvordan gjør man det:
```Elixir
# Åpne og lese en hel fil
{:ok, content} = File.read("example.txt")
IO.puts(content)

# Lese fil linje for linje
File.stream!("example.txt") |> Enum.each(&IO.puts(&1))
```
Eksempelutdata:
```
Dette er første linjen i filen.
Her er den neste linjen, og så videre.
```

## Dypdykk
I Elixir gjøres lesing av filer lett med innebygde moduler som `File`. Historisk sett har ulike språk tilbudt forskjellige måter å lese filer på, men Elixir's tilnærming er inspirert av Erlang's fokus på feilhåndtering og lette prosesser.

Som et alternativ til `File.read` og `File.stream!`, kan du bruke `File.open` etterfulgt av `IO.read` for mer kontroll, spesielt når du håndterer store filer eller binære data.

Elixir bruker binære trær for å representere tekst, noe som gjør det effektivt og raskt når en manipulerer store filer eller strømmer av data.

## Se Også
- [Elixir's offisielle dokumentasjon for File-modulen](https://hexdocs.pm/elixir/File.html)
- [Erlang's dokumentasjon om IO og filhåndtering](http://erlang.org/doc/man/file.html)
