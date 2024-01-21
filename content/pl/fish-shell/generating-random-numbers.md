---
title:                "Generowanie liczb losowych"
date:                  2024-01-20T17:49:28.045449-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generowanie liczb losowych"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Generowanie losowych liczb to proces tworzenia nieprzewidywalnych wartości. Programiści wykorzystują je w kryptografii, grach, testowaniu oprogramowania i symulacjach.

## How to: (Jak to zrobić:)
W Fish Shell, używamy `random` do generowania losowych liczb.

```Fish Shell
echo (random)
```

Wygeneruje losową liczbę między 1 a 32767.

Aby określić zakres:

```Fish Shell
echo (random 1 100)
```

Wygeneruje losową liczbę między 1 a 100.

Można też generować więcej niż jedną liczbę na raz:

```Fish Shell
echo (random 1 100 5)
```

Wygeneruje 5 losowych liczb w zakresie od 1 do 100.

## Deep Dive (Dogłębna analiza)
Fish używa pseudolosowej generatora liczb (PRNG), który inicjalizowany jest wartością startową (seed) – zwykle bieżącym czasem. W przeszłości `random` mógł być mniej przewidywalny. W innych językach i systemach do generowania liczb losowych używa się innych narzędzi, takich jak `/dev/random` w systemach Unix czy `rand()` w języku C.

Istotnym aspektem jest jakość generowanych liczb losowych – nie wszystkie PRNG są odpowiednie do każdego zastosowania. Na przykład, do zastosowań kryptograficznych potrzebne są generatory liczb losowych o wysokim stopniu nieprzewidywalności.

## See Also (Zobacz również)
- Dokumentacja Fish Shell na temat `random`: https://fishshell.com/docs/current/cmds/random.html
- Porównanie generatorów liczb losowych: https://en.wikipedia.org/wiki/Comparison_of_hardware_random_number_generators
- Informacje o PRNG w kontekście kryptografii: https://www.schneier.com/academic/archives/1996/01/pseudorandom_number.html