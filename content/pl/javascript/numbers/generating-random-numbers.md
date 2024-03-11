---
date: 2024-01-27 20:34:12.340976-07:00
description: "Generowanie losowych liczb w JavaScript to technika u\u017Cywana do\
  \ tworzenia nieprzewidywalno\u015Bci w aplikacjach, pocz\u0105wszy od gier, kt\xF3\
  re wymagaj\u0105 losowego\u2026"
lastmod: '2024-03-11T00:14:09.000844-06:00'
model: gpt-4-0125-preview
summary: "Generowanie losowych liczb w JavaScript to technika u\u017Cywana do tworzenia\
  \ nieprzewidywalno\u015Bci w aplikacjach, pocz\u0105wszy od gier, kt\xF3re wymagaj\u0105\
  \ losowego\u2026"
title: Generowanie liczb losowych
---

{{< edit_this_page >}}

## Co i dlaczego?

Generowanie losowych liczb w JavaScript to technika używana do tworzenia nieprzewidywalności w aplikacjach, począwszy od gier, które wymagają losowego zachowania wrogów, po algorytmy bezpieczeństwa wymagające kryptograficznej losowości. Ta zdolność jest kluczowa dla rozwijania dynamicznych doświadczeń użytkownika i bezpiecznych aplikacji.

## Jak to zrobić:

### Podstawowe generowanie liczby losowej

Najprostszym sposobem na wygenerowanie losowej liczby w JavaScript jest użycie `Math.random()`. Funkcja ta zwraca zmiennoprzecinkową, pseudolosową liczbę w zakresie od 0 (włącznie) do 1 (wyłącznie).

```javascript
let randomNumber = Math.random();
console.log(randomNumber);
```

### Generowanie losowej liczby w zadanym zakresie

Często chcesz uzyskać losową liczbę całkowitą w określonym zakresie. Można to osiągnąć, skalując i zaokrąglając wynik z `Math.random()`.

```javascript
function getRandomInt(min, max) {
  min = Math.ceil(min);
  max = Math.floor(max);
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

console.log(getRandomInt(1, 100));
```

### Kryptograficznie bezpieczne losowe liczby

Dla aplikacji wymagających wyższego stopnia losowości (np. operacje kryptograficzne), można użyć metody `crypto.getRandomValues()`. Zapewnia ona kryptograficzną losowość, w przeciwieństwie do pseudolosowych liczb generowanych przez `Math.random()`.

```javascript
(function generateSecureRandom() {
  let array = new Uint32Array(1);
  window.crypto.getRandomValues(array);
  console.log(array[0]);
})();
```

## Wnikliwe spojrzenie

Historycznie, generowanie losowych liczb w JavaScript opierało się wyłącznie na funkcji `Math.random()`. Choć wygodna dla większości przypadków użycia, jej algorytm, zazwyczaj wariant generatora pseudolosowego (PRNG) takiego jak Mersenne Twister, nie zapewnia bezpieczeństwa kryptograficznego.

Wprowadzenie Web Cryptography API przyniosło metodę `crypto.getRandomValues()`, oferującą sposób na generowanie liczb znacznie mniej przewidywalnych i odpowiednich dla aplikacji wrażliwych na bezpieczeństwo. Ta metoda korzysta z źródeł losowości wbudowanych w system operacyjny, takich jak `/dev/random` w Unix/Linux, które są bardziej solidne i odpowiednie dla operacji kryptograficznych.

Istotne jest wybranie odpowiedniej metody do zadania. `Math.random()` wystarcza do podstawowych potrzeb, takich jak proste gry, animacje czy każdy przypadek, gdzie jakość losowości nie jest krytyczna. Jednak dla funkcji bezpieczeństwa, takich jak tokeny resetujące hasło czy jakiekolwiek operacje kryptograficzne, `crypto.getRandomValues()` jest lepszym wyborem ze względu na jego wyższą jakość losowości.

Co ważne, `Math.random()` generuje liczby z znanym uprzedzeniem w większości implementacji, co oznacza, że niektóre liczby są bardziej prawdopodobne niż inne. Mimo że to uprzedzenie jest minimalne i często niewidoczne dla ogólnych zastosowań, dyskwalifikuje `Math.random()` do użycia w jakimkolwiek kontekście kryptograficznym lub aplikacjach, gdzie sprawiedliwość jest kluczowa, takich jak hazard online.

Podsumowując, choć wbudowane funkcje JavaScript do generowania losowych liczb pokrywają szeroki zakres potrzeb, zrozumienie różnic i ograniczeń każdej metody jest istotne dla ich odpowiedniego zastosowania.
