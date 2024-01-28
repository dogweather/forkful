---
title:                "Bruke et interaktivt skall (REPL)"
date:                  2024-01-26T04:14:32.086179-07:00
model:                 gpt-4-0125-preview
simple_title:         "Bruke et interaktivt skall (REPL)"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

En REPL, forkortelse for Read-Eval-Print Loop (Les-Evaluer-Utskriftsløkke), er et programmeringsverktøy for interaktivt å kjøre kode og se resultater umiddelbart. Programmerere bruker det til å eksperimentere, feilsøke, eller lære et nytt språk på sparket, som Gleam.

## Hvordan:

Gleam inkluderer for øyeblikket ikke en REPL i sin standarddistribusjon. Imidlertid kan du eksperimentere med Gleam-kode ved å bruke den eksisterende Erlang-skallet fordi Gleam kompilerer til Erlang-bytekode. Slik gjør du:

1. Kompiler din Gleam-kode til Erlang.
```plaintext
gleam build
```

2. Start Erlang-skallet.
```plaintext
erl -pa ebin
```

3. Kall dine Gleam-funksjoner (med forutsetning om at du har en modul kalt `my_mod` og funksjon `my_fun`).
```erlang
my_mod:my_fun().
```

Du burde se output fra din funksjon vist i skallet.

## Dypdykk

REPL omfavner den dynamiske og utforskende ånden til mange funksjonelle programmeringsspråk, som sporer tilbake til LISP's REPL på 1960-tallet. Sammenlignet tilbyr andre systemer som Python's `ipython` eller Ruby's `irb` lignende opplevelser for deres samfunn.

Selv om Gleam ikke har en innfødt REPL ennå, forblir utnyttelsen av Erlang-skallet en smart omvei. Erlang-skallets evner kommer fra BEAM VM, den virtuelle maskinen som driver Erlang-økosystemet, som inkluderer Elixir, LFE og Gleam.

Alternativer til REPLs i Gleam-økosystemet kunne inkludere å skrive testtilfeller eller bruke nettbaserte kompilatorer og kodelekeplasser som støtter Gleam, for å teste ut kodebiter utenfor en full prosjektoppsett.

Implementeringen av en dedikert Gleam REPL møter utfordringer hovedsakelig rundt den kompilerte naturen til Gleam og Erlangs kjøretidsmiljø, hvor varm kodebytting er normen. En eventuell fremtidig Gleam REPL måtte forsone språkets statiske typetilkning med det dynamiske utførelsesmiljøet en REPL forventer.

## Se Også

- Gleams offisielle dokumentasjon: https://gleam.run/book/
- Erlangs skall-dokumentasjon: http://erlang.org/doc/man/erl.html
- En nettbasert Gleam-kompilator-lekeplass: https://gleam.run/compiler/
