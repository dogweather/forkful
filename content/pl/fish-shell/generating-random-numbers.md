---
title:                "Generowanie liczb losowych"
html_title:           "Gleam: Generowanie liczb losowych"
simple_title:         "Generowanie liczb losowych"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Generowanie liczb losowych to proces tworzenia liczb, które nie mają żadnego dostrzegalnego wzorca czy przewidywalności. Programiści używają ich do tworzenia skomplikowanych algorytmów, gier, symulacji i wielu innych zastosowań, które potrzebują nieprzewidywalności.

## Jak to zrobić:
Tworzenie losowych liczb w Fish Shell jest proste jak bułka z masłem. Zwyczajnie użyj polecenia `random`.

```Fish Shell
random 1 100
```
Zwróci losową liczbę między 1 a 100.

Wyjście:
```Fish Shell
45
```
To jest tylko jeden przykład. Możesz dostosować dane wejściowe do swoich potrzeb.

## Głębsze zanurzenie:
Historia generowania liczb losowych w programowaniu jest zaskakująco bogata. Właściwie, losowość stała się kluczowym elementem w wielu dziedzinach informatyki. W przeciwieństwie do innych powłok Unixowych, takich jak bash czy zsh, Fish Shell korzysta z stosunkowo nowego systemu do generowania liczb pseudolosowych, który gwarantuje lepszą losowość. Alternatywą dla wbudowanej funkcji `random` jest użycie zewnętrznych narzędzi, takich jak /dev/random lub /dev/urandom. 

Szczegółowo, `random` w Fish Shell działa poprzez wykorzystanie wewnętrznej funkcji `rand()`, która zwraca liczbę pseudolosową. Ale pamiętaj, że jest to pseudolosowość, co oznacza, że nie jest to prawdziwa losowość i nie powinna być używana w przypadkach, które wymagają prawdziwej losowości, takich jak kryptografia.

## Zobacz również:
- [Oficjalna dokumentacja Fish Shell](https://fishshell.com/docs/current/index.html)
- [Wikipedii na temat generowania liczb pseudolosowych](https://pl.wikipedia.org/wiki/Generator_liczb_pseudolosowych)