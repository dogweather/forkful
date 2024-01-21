---
title:                "Å starte et nytt prosjekt"
date:                  2024-01-20T18:03:32.101385-07:00
model:                 gpt-4-1106-preview
simple_title:         "Å starte et nytt prosjekt"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why?
Å starte et nytt prosjekt betyr å skape en helt ny mappestruktur og kodebase for idéen din. Programmerere gjør dette for å realisere unike løsninger eller teste ut ideer.

## How to:
For å kickstarte et Gleam-prosjekt, installer `gleam` og kjør:

```bash
gleam new mitt_kule_prosjekt
```

Dette lager en ny mappe `mitt_kule_prosjekt` med en standard mappestruktur. For å teste, naviger til prosjektmappe og bygg prosjektet:

```Gleam
cd mitt_kule_prosjekt
gleam build
```

Sample output:

```
Compiling mitt_kule_prosjekt
Generated src/mitt_kule_prosjekt-app.erl
Done!
```

## Deep Dive
Gleam dukket opp i 2018, laget av Louis Pilfold, som en type-sikker funksjonell programmeringsspråk bygget på Erlang's robuste VM. Kontra alternativer som Elixir og Erlang, streber Gleam etter mer nøyaktig typehåndtering. Nytt prosjekt starter med `gleam new` og etablerer en OTP-applikasjonsstruktur, kjent fra Erlang-økosystemet. Det gir OOP-lignende funksjoner (via moduler og funksjoner) med funksjonell klarhet.

## See Also
- Gleam's offisielle hjemmeside: [https://gleam.run/](https://gleam.run/)
- Gleam's GitHub-repositorium: [https://github.com/gleam-lang/gleam](https://github.com/gleam-lang/gleam)
- Erlang/OTP konsepter: [https://www.erlang.org/doc/](https://www.erlang.org/doc/)