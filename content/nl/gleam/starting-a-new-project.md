---
title:                "Een nieuw project starten"
date:                  2024-01-28T22:08:32.207442-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een nieuw project starten"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/gleam/starting-a-new-project.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Een nieuw project starten betekent het initialiseren van een nieuwe codebasis met de nodige structuur en configuratie. Programmeurs doen dit om de ontwikkeling met een schone lei te beginnen, waarbij ze vanaf het begin zorgen voor de beste praktijken en organisatie.

## Hoe te:

Om een nieuw Gleam-project te maken, heb je de `gleam` opdrachtregeltool nodig. Installeer deze en voer vervolgens uit:

```shell
gleam new my_cool_project
```

Dit creëert een nieuwe map met de naam `my_cool_project` met een basisprojectstructuur:

```plaintext
my_cool_project/
├── gleam.toml
├── src
│   └── my_cool_project.gleam
└── test
    └── my_cool_project_test.gleam
```

De map `src` bevat de hoofdmodule van Gleam, en de map `test` is waar je tests zullen staan. Laten we eens kijken naar de standaard `my_cool_project.gleam`:

```gleam
pub fn hello_world() {
  "Hallo, wereld!"
}
```

Zoet en eenvoudig. Nu heb je een nieuw Gleam-project gestart!

## Diepere Duik

Gleam kwam rond 2018 op de markt met als doel sterke statische typen naar het Erlang-ecosysteem te brengen - zonder de beroemde betrouwbaarheid en concurrency model te verliezen.

Alternatieven voor het starten van een project met `gleam new` kunnen het klonen van een sjabloon uit een repository of het handmatig creëren van de bestandsstructuur omvatten. Het gebruik van de Gleam-tool biedt echter een consistente startpunt, en het is ontworpen om naadloos binnen het ecosysteem te werken.

Achter de schermen zet `gleam new` een `rebar3` of `mix` project op, afhankelijk van je voorkeur (de standaard is `rebar3`). Het vult de noodzakelijke configuratiebestanden in, zoals `gleam.toml` voor afhankelijkheidsbeheer en projectinstellingen, en `rebar.config` of `mix.exs` voor respectievelijk Erlang- of Elixir-interoperabiliteit.

## Zie Ook

- De officiële startgids van Gleam: [https://gleam.run/book/getting-started/](https://gleam.run/book/getting-started/)
- GitHub-repository van Gleam: [https://github.com/gleam-lang/gleam](https://github.com/gleam-lang/gleam)
- Documentatie van Gleams standaardbibliotheek: [https://hexdocs.pm/gleam_stdlib/](https://hexdocs.pm/gleam_stdlib/)
