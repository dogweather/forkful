---
title:                "Generowanie liczb losowych"
html_title:           "Gleam: Generowanie liczb losowych"
simple_title:         "Generowanie liczb losowych"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Generowanie losowych liczb to process, w którym tworzy się unikatowe liczby w sposób nieprzewidywalny. Programiści robią to między innymi do symulowania przypadkowego zachowania, tworzenia danych testowych lub wprowadzania elementu zaskoczenia.

## Jak to zrobić:

Tu jest jak możemy wygenerować losową liczbę przy użyciu TypeScript:

``` typescript
function get_random_int(min: number, max: number): number {
    return Math.floor(Math.random() * (max - min + 1) ) + min;
}

console.log(get_random_int(1, 6));
```

Wynik powyższego kodu będzie losową liczbą z zakresu od 1 do 6.

## Głębsze zanurzenie

Generowanie losowych liczb ma długą historię w informatyce. Zarówno zastosowania, jak i metody generowania tych liczb ewoluowały na przestrzeni lat. W TypeScript korzystamy z wbudowanej funkcji Math.random(), ale jest wiele alternatywnych sposobów generowania losowych liczb w różnych językach programowania.

Jednym z nich jest użycie generatora liczb pseudolosowych (PRNG). PRNG jest algorytmem, który generuje sekwencję liczb, które wydają się losowe, ale są w rzeczywistości określone przez początkowy "ziarno" (seed). Valuable w grach dla powtarzalności.

Jeszcze innym podejściem jest użycie prawdziwego generatora liczb losowych (TRNG). TRNG są zazwyczaj oparte na fizycznych zjawiskach i są bardziej nieprzewidywalne niż PRNG, ale mogą być trudniejsze do zaimplementowania.

## Zobacz również

[Generowanie liczb losowych - Wikipedia](https://en.wikipedia.org/wiki/Random_number_generation)

[Random number generation - MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)

[Dokumentacja TypeScript](https://www.typescriptlang.org/docs/)