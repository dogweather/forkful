---
title:                "Å starte et nytt prosjekt"
html_title:           "C: Å starte et nytt prosjekt"
simple_title:         "Å starte et nytt prosjekt"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Komme i Gang med Elixir: Hvordan Starter man et Nytt Prosjekt!

## Hva & Hvorfor?
Å starte et nytt prosjekt betyr å skape en ny, frisk utviklingsarena der programmer fungere og skapes. Vi gjør dette for å organisere og adskille forskjellige applikasjoner eller komponenter vi jobber med, noe som fører til mer effektiv og mindre forvirrende programmering.

## Hvordan Gjøre Det:
I Elixir starter vi et nytt prosjekt med Mix, en bygningsverktøy som leveres med Elixir som hjelper deg med å lage, utvikle og teste dine prosjekter.

Her er en kodeeksempel på hvordan opprette et helt nytt Elixir-prosjekt:

```Elixir
# Starter et nytt mix-prosjekt
mix new my_project

# Navigerer til prosjektet
cd my_project
```

Dette vil lage et nytt Elixir-prosjekt med navnet 'my_project'. Det lager også en grunnleggende mappestruktur og noen viktige filer som README.md, .formatter.exs og mix.exs filene.

## Dypdykk
Historisk har det å begynne et nytt prosjekt i mange programmeringsspråk, inkludert eldre versjoner av Elixir, vært mye mer arbeidskrevende. Før Mix, hadde utviklere å manuelt sette opp filer og mapper, noe som kan være tidkrevende og feilfôr.

Alternativer til 'mix' eksisterer, som rebar3 for Erlang, men Mix er innebygd i Elixir og er derfor det mest brukt for Elixir-prosjekter.

Mix gjør ikke bare å starte nye prosjekter enklere. Det hjelper deg også med å håndtere avhengigheter, kjøre tester, og har mange andre nyttige funksjoner som er godt verdt å utforske.

## Se Også
1. Offisielt Elixir nettsted - https://elixir-lang.org/
2. Mix dokumentasjon - https://hexdocs.pm/mix/Mix.html
3. Elixir School - https://elixirschool.com/ 
4. Getting Started guide i Elixir Docs - https://elixir-lang.org/getting-started/introduction.html

Elixir er et kraftig, pålitelig og skalerbart programmeringsspråk. Ved å bruke Mix for å styre prosjektene dine, kan du effektivisere arbeidsflyten din og fokusere på selve programmeringen.