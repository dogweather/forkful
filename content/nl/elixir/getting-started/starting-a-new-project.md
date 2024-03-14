---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:17.841212-07:00
description: "Een nieuw Elixir-project starten betekent het opzetten van een frisse\
  \ Mix-projectstructuur. Programmeurs doen dit om te beginnen met georganiseerde\
  \ code\u2026"
lastmod: '2024-03-13T22:44:50.462784-06:00'
model: gpt-4-0125-preview
summary: "Een nieuw Elixir-project starten betekent het opzetten van een frisse Mix-projectstructuur.\
  \ Programmeurs doen dit om te beginnen met georganiseerde code\u2026"
title: Een nieuw project starten
---

{{< edit_this_page >}}

## Wat & Waarom?
Een nieuw Elixir-project starten betekent het opzetten van een frisse Mix-projectstructuur. Programmeurs doen dit om te beginnen met georganiseerde code en geautomatiseerde hulpmiddelen, die helpen bij het efficiënt bouwen en testen van hun Elixir-applicaties.

## Hoe:
Om een nieuw project te creëren, gebruik je het `mix new` commando:

```elixir
$ mix new my_app
```

Je zult zoiets als dit zien:

```
* creating README.md
* creating .formatter.exs
* creating .gitignore
* creating mix.exs
* creating lib
* creating lib/my_app.ex
* creating test
* creating test/test_helper.exs
* creating test/my_app_test.exs
```

Ga naar je nieuwe projectdirectory:

```elixir
$ cd my_app
```

Nu kun je je project of de tests ervan uitvoeren:

Je project uitvoeren:

```elixir
$ iex -S mix
```
Testen:

```elixir
$ mix test
```

## Diepgaande Duik
De bouwtool van Elixir, Mix, is ontstaan uit de wens om een betrouwbare en uniforme manier te bieden om projecten te creëren, te configureren, en te beheren. Het is beïnvloed door hulpmiddelen uit andere ecosystemen, zoals Bundler en Rake van Ruby. Mix brengt afhankelijkheidsbeheer en taakautomatisering in de gereedschapskist van Elixir. De alternatieven in andere talen kunnen npm voor Node.js of Maven voor Java zijn. Echter, Mix is afgestemd op de Elixir-runtime en geïntegreerd met zijn idiomatische patronen. Het `mix new` commando maakt een conventionele structuur met vooraf gedefinieerde directories en bestanden, zoals configuratiebestanden, moduledefinities, en testsuites. Het volgen van conventies is cruciaal in Elixir; het moedigt codeconsistentie en leesbaarheid aan over Elixir-projecten.

## Zie Ook
- Officiële `mix` documentatie: [https://hexdocs.pm/mix/Mix.html](https://hexdocs.pm/mix/Mix.html)
- Elixir School's projectgids: [https://elixirschool.com/en/lessons/basics/mix/](https://elixirschool.com/en/lessons/basics/mix/)
