---
date: 2024-01-27 20:35:05.004318-07:00
description: "Jak to zrobi\u0107: PHP oferuje kilka funkcji do generowania losowych\
  \ liczb, ale najcz\u0119\u015Bciej u\u017Cywane to `rand()`, `mt_rand()` i, do cel\xF3\
  w kryptograficznych,\u2026"
lastmod: '2024-03-13T22:44:35.492972-06:00'
model: gpt-4-0125-preview
summary: "PHP oferuje kilka funkcji do generowania losowych liczb, ale najcz\u0119\
  \u015Bciej u\u017Cywane to `rand()`, `mt_rand()` i, do cel\xF3w kryptograficznych,\
  \ `random_int()`."
title: Generowanie liczb losowych
weight: 12
---

## Jak to zrobić:
PHP oferuje kilka funkcji do generowania losowych liczb, ale najczęściej używane to `rand()`, `mt_rand()` i, do celów kryptograficznych, `random_int()`.

Aby wygenerować prostą losową liczbę między 0 a getrandmax() (największa możliwa wartość zwracana przez `rand()`), możesz użyć:

```PHP
echo rand();
```

Dla bardziej określonego zakresu, takiego jak między 1 a 100:

```PHP
echo rand(1, 100);
```

Jednak `mt_rand()` jest lepszym wyborem ze względu na szybkość i losowość:

```PHP
echo mt_rand(1, 100);
```

Wynik dla obu może być czymś między 1 a 100, w zależności od losowości, np. `42`.

Dla kontekstów kryptograficznych lub bezpieczeństwa, gdzie nieprzewidywalność jest kluczowa, preferowanym wyborem jest `random_int()`, ponieważ generuje kryptograficznie bezpieczne pseudolosowe liczby całkowite:

```PHP
echo random_int(1, 100);
```

Ponownie, wynik to losowa liczba między 1 a 100, jak `84`, ale z silniejszą gwarancją losowości.

## Wgłębienie się
Funkcja `rand()` była obecna w PHP od jego wczesnych wersji, służąc jako początkowe podejście do generowania losowych liczb. Jednak nie jest najlepszym wyborem dla aplikacji wymagających wysokiego stopnia losowości ze względu na jej stosunkowo przewidywalny algorytm.

`mt_rand()`, wprowadzony w PHP 4, opiera się na algorytmie Mersenne Twister - znacznie lepszy pod względem szybkości i generowanej losowości w porównaniu do `rand()`. Szybko stał się preferowaną opcją dla większości potrzeb niekryptograficznych.

Dla aplikacji wrażliwych na bezpieczeństwo, `random_int()` został wprowadzony w PHP 7 do generowania kryptograficznie bezpiecznych pseudolosowych liczb całkowitych, używając losowych bajtów z generatora liczb losowych systemu. Jest znacznie bezpieczniejszy niż `rand()` czy `mt_rand()`, co czyni go najlepszym wyborem do generowania tokenów, kluczy czy innych elementów, gdzie przewidywalność mogłaby prowadzić do luk w bezpieczeństwie.

Pomimo tych ulepszeń, kluczowe jest wybranie właściwej funkcji w zależności od kontekstu aplikacji. Do ogólnego użytku wystarcza `mt_rand()`, ale do czegoś, co mogłoby być celem ataków lub wykorzystane, `random_int()` jest najlepszą drogą, zapewniając zarówno losowość, jak i bezpieczeństwo.
