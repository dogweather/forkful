---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:17.590097-07:00
description: "Het schrijven van een tekstbestand houdt in dat gegevens worden opgeslagen\
  \ in een bestand in een door mensen leesbaar formaat. Programmeurs doen dit om\u2026"
lastmod: '2024-03-11T00:14:24.294031-06:00'
model: gpt-4-0125-preview
summary: "Het schrijven van een tekstbestand houdt in dat gegevens worden opgeslagen\
  \ in een bestand in een door mensen leesbaar formaat. Programmeurs doen dit om\u2026"
title: Een tekstbestand schrijven
---

{{< edit_this_page >}}

## Wat & Waarom?

Het schrijven van een tekstbestand houdt in dat gegevens worden opgeslagen in een bestand in een door mensen leesbaar formaat. Programmeurs doen dit om informatie zoals logs, instellingen en data-output te bewaren.

## Hoe te:

Elixir maakt het schrijven van tekstbestanden eenvoudig. Hier is een simpel voorbeeld van het schrijven naar een bestand genaamd "hello.txt".

```elixir
File.write("hello.txt", "Hallo, Wereld!\n")
```
Na dit uitgevoerd te hebben, controleer "hello.txt" en het zal lezen:

```
Hallo, Wereld!
```
Voor het toevoegen van tekst in plaats van overschrijven:

```elixir
File.write("hello.txt", "Nog een regel!\n", [:append])
```

Nu zal "hello.txt" tonen:

```
Hallo, Wereld!
Nog een regel!
```

## Diepere Duik

Elixir's benadering van het schrijven van bestanden weerspiegelt zijn Erlang-erfgoed, met een focus op betrouwbaarheid en gelijktijdigheid. Alternatieven omvatten het gebruik van streams voor grotere gegevens. Intern gebruikt Elixir Erlang's :file module die interageert met het onderliggende besturingssysteem.

## Zie Ook

- Elixir `File` module documentatie: https://hexdocs.pm/elixir/File.html
- Erlang `:file` module documentatie: https://erlang.org/doc/man/file.html
- Leer over Elixir's Stream module voor het omgaan met grote data: https://hexdocs.pm/elixir/Stream.html
