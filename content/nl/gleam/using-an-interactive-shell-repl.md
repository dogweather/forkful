---
title:                "Het gebruik van een interactieve shell (REPL)"
date:                  2024-01-28T22:09:13.996210-07:00
model:                 gpt-4-0125-preview
simple_title:         "Het gebruik van een interactieve shell (REPL)"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/gleam/using-an-interactive-shell-repl.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Een REPL, een afkorting voor Read-Eval-Print Loop, is een programmeerhulpmiddel voor het interactief uitvoeren van code en het direct zien van resultaten. Programmeurs gebruiken het om te experimenteren, debuggen, of om een nieuwe taal ter plaatse te leren, zoals Gleam.

## Hoe te:

Gleam bevat momenteel geen REPL in zijn standaarddistributie. Je kunt echter experimenteren met Gleam-code met behulp van de bestaande Erlang-shell, omdat Gleam compileert naar Erlang bytecode. Zo gaat het in zijn werk:

1. Compileer je Gleam-code naar Erlang.
```plaintext
gleam build
```

2. Start de Erlang-shell.
```plaintext
erl -pa ebin
```

3. Roep je Gleam-functies aan (ervan uitgaande dat je een module hebt genaamd `my_mod` en functie `my_fun`).
```erlang
my_mod:my_fun().
```

Je zou de output van je functie in de shell moeten zien.

## Diepere Duik

REPL belichaamt de dynamische en verkennende geest van vele functionele programmeertalen, teruggaand tot de REPL van LISP in de jaren 1960. Vergelijkbaar bieden andere systemen zoals Python's `ipython` of Ruby's `irb` soortgelijke ervaringen voor hun gemeenschappen.

Hoewel Gleam nog geen native REPL heeft, blijft het gebruik van de Erlang-shell een handige oplossing. De mogelijkheden van de Erlang-shell komen van de BEAM VM, de virtuele machine die het Erlang-ecosysteem aandrijft, dat Elixir, LFE en Gleam omvat.

Alternatieven voor REPLs in het Gleam-ecosysteem zouden kunnen omvatten het schrijven van testgevallen of het gebruik van online compilers en code speelplaatsen die Gleam ondersteunen, om fragmenten van code te testen buiten een volledige projectopstelling.

De implementatie van een toegewijde Gleam REPL wordt voornamelijk uitgedaagd door de gecompileerde aard van Gleam en de runtime van Erlang, waar het normaal is om code 'hot' te swappen. Een toekomstige Gleam REPL zou de statische typen van de taal moeten verzoenen met de dynamische uitvoeringsomgeving die een REPL verwacht.

## Zie Ook

- Officiële documentatie van Gleam: https://gleam.run/book/
- Documentatie van de Erlang-shell: http://erlang.org/doc/man/erl.html
- Een online Gleam compiler speelplaats: https://gleam.run/compiler/
