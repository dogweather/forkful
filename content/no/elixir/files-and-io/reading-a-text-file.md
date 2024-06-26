---
date: 2024-01-20 17:53:55.205184-07:00
description: "Hvordan gj\xF8r man det: I Elixir gj\xF8res lesing av filer lett med\
  \ innebygde moduler som `File`. Historisk sett har ulike spr\xE5k tilbudt forskjellige\
  \ m\xE5ter \xE5\u2026"
lastmod: '2024-04-05T22:50:54.471631-06:00'
model: gpt-4-1106-preview
summary: "I Elixir gj\xF8res lesing av filer lett med innebygde moduler som `File`."
title: Lese en tekstfil
weight: 22
---

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
