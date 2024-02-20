---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:59.747594-07:00
description: "Een tekstbestand lezen betekent gegevens uit een bestand in je programma\
  \ trekken. Programmeurs doen dit om de inhoud te verwerken of te analyseren, zoals\u2026"
lastmod: 2024-02-19 22:05:09.571457
model: gpt-4-0125-preview
summary: "Een tekstbestand lezen betekent gegevens uit een bestand in je programma\
  \ trekken. Programmeurs doen dit om de inhoud te verwerken of te analyseren, zoals\u2026"
title: Een tekstbestand lezen
---

{{< edit_this_page >}}

## Wat & Waarom?

Een tekstbestand lezen betekent gegevens uit een bestand in je programma trekken. Programmeurs doen dit om de inhoud te verwerken of te analyseren, zoals het lezen van configuraties, het analyseren van logs of het importeren van gegevens.

## Hoe:

Hier is hoe je de volledige inhoud van een tekstbestand genaamd `example.txt` leest:

```elixir
File.read("example.txt")
```

Voorbeelduitvoer als `example.txt` "Hello, Elixir!" bevat:

```elixir
{:ok, "Hello, Elixir!"}
```

Om het bestand regel voor regel te lezen:

```elixir
File.stream!("example.txt")
|> Enum.each(fn line -> IO.puts(line) end)
```

Dit zal elke regel van `example.txt` naar de console printen.

## Diepere Duik

In Elixir zijn `File.read/1` en `File.stream!/1` typische manieren om tekstbestanden te lezen. Historisch gezien komt het lezen van bestanden in de programmering voort uit de behoefte om gegevens op te slaan en op te halen. In de vroege computertechniek werd dit gedaan met behulp van ponskaarten of magnetische tapes. Tegenwoordig gebruiken we verschillende opslagapparaten zoals SSD's, HDD's en meer.

Een alternatief voor `File.read/1` is `File.read!/1`, dat een foutmelding geeft als er iets misgaat, in plaats van een tuple terug te geven. Op dezelfde manier verschilt `File.stream!/1` van `File.stream/1` door een foutmelding te geven bij mislukking in plaats van een fouttuple te retourneren.

De implementatie onder de motorkap gaat om met binaire gegevens. Tekst wordt door Elixir omgezet in binaries, die de onderliggende bytes en codering afhandelt.

## Zie Ook:

- De officiële `File` module documentatie van Elixir: [https://hexdocs.pm/elixir/File.html](https://hexdocs.pm/elixir/File.html)
