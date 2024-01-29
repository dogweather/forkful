---
title:                "Met een debugger werken"
date:                  2024-01-28T22:16:56.125102-07:00
model:                 gpt-4-0125-preview
simple_title:         "Met een debugger werken"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/gleam/using-a-debugger.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Een debugger gebruiken is in feite alsof je detective speelt in je code, op zoek gaat naar bugs en uitzoekt waarom dingen niet soepel lopen. Programmeurs doen het omdat, laten we eerlijk zijn, bugs onvermijdelijk zijn, en ze efficiënt uit de weg ruimen betekent dat je code sneller en betrouwbaarder draait.

## Hoe:
Gleam steunt momenteel op het Erlang-ecosysteem voor gereedschap, dus je zult typisch debuggen met tools zoals `rebar3`, `observer` en `debugger`. Zo ga je grondig te werk met debuggen:

```gleam
// Zorg in je rebar-configuratie dat je deze regels hebt om debug-informatie in te sluiten:
{erl_opts, [debug_info]}.

// Start een Erlang-shell met je app geladen
rebar3 shell

// In de shell kun je de debugger starten
1> debugger:start().
```

Eenvoudig, toch? De `debugger` GUI verschijnt, en je kunt breakpoints instellen, door de code stappen en variabelen bekijken zoveel als je wilt. Je zult de Gleam-code niet direct zien, maar wel de Erlang-code waarnaar het compileert, wat nog steeds behoorlijk handig is.

## Diepere Duik
Gleam is een jonge taal, dus hoewel het profiteert van het Erlang-ecosysteem, zijn native Gleam-debuggereedschappen nog niet in de schijnwerpers. Dat betekent dat we de beproefde tools van Erlang gebruiken, en dat is geen slechte zaak. Erlangs debugger bestaat al sinds de jaren '90, geslepen door jaren van vervelende bugs uitroeien in systemen waar betrouwbaarheid cruciaal is.

Wat betreft alternatieven, tracing is een krachtige methode in de BEAM-wereld (dat is de virtuele machine die Erlang en Elixir-code draait). Met `rebar3` kun je gebruik maken van tools zoals `recon` om functieaanroepen te traceren en diep in te gaan op prestatieproblemen.

De wisseling tussen het schrijven van Gleam en debuggen in Erlang kan voelen alsof je je gedachten ter plekke vertaalt. Maar het voordeel is dat je een kijkje krijgt in de wereld van Erlang, en de bouwstenen van je app in zijn runtime-vorm begrijpt.

## Zie Ook
Om je debugginggereedschapskist uit te breiden, bekijk:

- Erlangs debuggerdocumentatie: [https://erlang.org/doc/apps/debugger/debugger_chapter.html](https://erlang.org/doc/apps/debugger/debugger_chapter.html)
- De `recon` bibliotheek voor Erlang: [https://ferd.github.io/recon/](https://ferd.github.io/recon/)
