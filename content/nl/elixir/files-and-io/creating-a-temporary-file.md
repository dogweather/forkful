---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:05.926269-07:00
description: "Een tijdelijk bestand cre\xEBren betekent het maken van een bestand\
  \ dat je weggooit na kort gebruik. Programmeurs doen dit voor tijdelijke opslag\
  \ of wanneer\u2026"
lastmod: '2024-03-13T22:44:50.481354-06:00'
model: gpt-4-0125-preview
summary: "Een tijdelijk bestand cre\xEBren betekent het maken van een bestand dat\
  \ je weggooit na kort gebruik. Programmeurs doen dit voor tijdelijke opslag of wanneer\u2026"
title: Een tijdelijk bestand aanmaken
---

{{< edit_this_page >}}

## Wat & Waarom?
Een tijdelijk bestand creëren betekent het maken van een bestand dat je weggooit na kort gebruik. Programmeurs doen dit voor tijdelijke opslag of wanneer ze willen voorkomen dat een harde schijf verstopt raakt met gegevens die een zeer korte houdbaarheid hebben.

## Hoe te:
In Elixir kun je een tijdelijk bestand maken en gebruiken met de functie `System.tmp_dir/1` en de module `File`. Hier is een snel voorbeeld:

```elixir
# Laten we de mouwen opstropen en aan de slag gaan!

# Vind de tijdelijke map
temp_dir = System.tmp_dir!()

# Maak een pad voor het tijdelijke bestand
temp_file_path = Path.join(temp_dir, "mijn_tijdelijk_bestand.txt")

# Laten we iets tijdelijks schrijven
File.write!(temp_file_path, "Hallo, tijdelijke wereld!")

# Lees het, om er zeker van te zijn dat alles in orde is
IO.puts(File.read!(temp_file_path))

# Ruim na onszelf op en verwijder het tijdelijke bestand
File.rm!(temp_file_path)
```

Voorbeelduitvoer:
```
Hallo, tijdelijke wereld!
```

## Diepere Duik
Tijdelijke bestanden zijn niet uniek voor Elixir. Ze zijn een standaard in programmeertalen omdat ze perfect zijn voor het omgaan met gegevens die alleen van belang zijn tijdens de uitvoering van een programma. Voordat opslag goedkoop werd, was het sparen van schijfruimte cruciaal - tijdelijke bestanden hielpen daarbij. Tegenwoordig zijn ze handig voor het beheren van middelen en beveiliging: minder permanente gegevens betekent minder sporen achterlaten.

Wat betreft alternatieven, in Elixir, zou je je eigen tijdelijke bestandslogica kunnen opzetten of direct Erlang-functies kunnen gebruiken (bijv. `:erlang.mktemp/0`). En over details, wanneer je een tijdelijk bestand maakt, worden de details - zoals naamgeving - afgehandeld door je OS, niet door Elixir zelf. Elixir vraagt alleen aan het OS waar het bestand tijdelijk op te slaan en het OS reageert.

## Zie Ook
Voor meer Elixir bestandmanipulaties:
- Elixir's `File` module: https://hexdocs.pm/elixir/File.html
- Officiële docs voor `System.tmp_dir/1`: https://hexdocs.pm/elixir/System.html#tmp_dir/1

Het verkennen van Erlang's bestandsbeheermogelijkheden:
- Erlang's `file` module: http://erlang.org/doc/man/file.html
