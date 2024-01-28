---
title:                "Korzystanie z interaktywnego shella (REPL)"
date:                  2024-01-26T04:14:40.613853-07:00
model:                 gpt-4-0125-preview
simple_title:         "Korzystanie z interaktywnego shella (REPL)"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

REPL, skrót od Read-Eval-Print Loop (Pętla Czytaj-Wykonaj-Wypisz), to narzędzie programistyczne do interaktywnego uruchamiania kodu i natychmiastowego oglądania wyników. Programiści używają go do eksperymentowania, debugowania lub nauki nowego języka „w locie”, jakim jest Gleam.

## Jak to zrobić:

Gleam obecnie nie zawiera REPL w swojej standardowej dystrybucji. Możesz jednak eksperymentować z kodem Gleam, używając istniejącej powłoki Erlanga, ponieważ Gleam kompiluje się do bajtkodu Erlanga. Oto jak:

1. Skompiluj swój kod Gleam do Erlanga.
```plaintext
gleam build
```

2. Uruchom powłokę Erlanga.
```plaintext
erl -pa ebin
```

3. Wywołaj swoje funkcje Gleam (zakładając, że masz moduł o nazwie `my_mod` i funkcję `my_fun`).
```erlang
my_mod:my_fun().
```

Powinieneś zobaczyć wynik swojej funkcji wyświetlony w powłoce.

## Pogłębiona analiza

REPL uosabia dynamicznego i eksploracyjnego ducha wielu języków programowania funkcjonalnego, sięgając swoimi korzeniami do REPL-a LISP z lat 60. XX wieku. Dla porównania, inne systemy jak `ipython` Pythona czy `irb` Ruby'ego oferują podobne doświadczenia dla swoich społeczności.

Chociaż Gleam jeszcze nie posiada natywnego REPL, wykorzystanie powłoki Erlanga pozostaje sprytnym obejściem. Możliwości powłoki Erlanga wynikają z wirtualnej maszyny BEAM, która napędza ekosystem Erlanga, i której używają również Elixir, LFE i Gleam.

Alternatywy dla REPL w ekosystemie Gleam mogłyby obejmować pisanie przypadków testowych lub korzystanie z kompilatorów online i placów zabaw do kodu, które obsługują Gleam, aby testować fragmenty kodu poza pełną konfiguracją projektu.

Implementacja dedykowanego REPL dla Gleam napotyka wyzwania głównie ze względu na skompilowaną naturę Gleam i środowisko uruchomieniowe Erlanga, gdzie normą jest gorąca wymiana kodu. Każdy przyszły REPL dla Gleam musiałby pogodzić statyczne typowanie języka z dynamicznym środowiskiem wykonawczym, którego oczekuje REPL.

## Zobacz również

- Oficjalna dokumentacja Gleam: https://gleam.run/book/
- Dokumentacja powłoki Erlanga: http://erlang.org/doc/man/erl.html
- Plac zabaw kompilatora Gleam online: https://gleam.run/compiler/
