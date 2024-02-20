---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:03.421014-07:00
description: "De huidige datum opvragen in een programma is alsof je vraagt: \"H\xE9\
  , wat is de datum van vandaag?\" We doen dit om gebeurtenissen te tijdstempelen,\u2026"
lastmod: 2024-02-19 22:05:09.564197
model: gpt-4-0125-preview
summary: "De huidige datum opvragen in een programma is alsof je vraagt: \"H\xE9,\
  \ wat is de datum van vandaag?\" We doen dit om gebeurtenissen te tijdstempelen,\u2026"
title: Het huidige datum ophalen
---

{{< edit_this_page >}}

## Wat & Waarom?
De huidige datum opvragen in een programma is alsof je vraagt: "Hé, wat is de datum van vandaag?" We doen dit om gebeurtenissen te tijdstempelen, planningen te beheren, of gewoon om gebruikers te laten zien welke dag het is.

## Hoe:
Elixir maakt het verkrijgen van de huidige datum eenvoudig met behulp van de `Date` module:

```elixir
# Haal de huidige datum op
huidige_datum = Date.utc_today()

# Druk deze af
IO.inspect(huidige_datum)
```

Voorbeelduitvoer:

```elixir
~D[2023-04-06]
```

## Diepere duik
Vroeger hadden programmeurs te maken met primitievere talen en moesten ze handmatig data berekenen op basis van seconden sinds een tijdperk (meestal 1 januari 1970). Tegenwoordig biedt Elixir de `Date` module, die de omgang met data vereenvoudigt.

Alternatieven zijn het gebruik van `DateTime.utc_now()` als je de exacte tijd naast de datum nodig hebt, of `NaiveDateTime.local_now()` als je werkt met lokale tijd zonder tijdzone-informatie.

Onder de motorkap vertrouwt Elixir op Erlangs mogelijkheden voor tijdbeheer. Wanneer je `Date.utc_today()` aanroept, interfaceert het met Erlang om de gecoördineerde wereldtijd (UTC) te krijgen.

## Zie ook
- Documentatie van Elixirs `Date` module: https://hexdocs.pm/elixir/Date.html
- Elixirs `DateTime` module voor complexere tijdgerelateerde taken: https://hexdocs.pm/elixir/DateTime.html
- Introductie tot `NaiveDateTime`: https://hexdocs.pm/elixir/NaiveDateTime.html
