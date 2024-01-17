---
title:                "Znajdowanie długości ciągu znaków"
html_title:           "TypeScript: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Co to jest i dlaczego to robimy?

Znalezienie długości ciągu znaków jest jedną z podstawowych operacji, które często wykonywane są przez programistów. Polega ona na określeniu liczby znaków znajdujących się w danym ciągu. Jest to przydatne w wielu sytuacjach, na przykład podczas pracy z tekstami lub analizy danych.

## Jak to zrobić?

```TypeScript
let string = "Hello World";

console.log(string.length);

// Output: 11
```

Możemy wykorzystać wbudowaną metodę ```length```, która jest dostępna dla każdego ciągu znaków w języku TypeScript. W powyższym przykładzie, używając słowa kluczowego ```let``` definiujemy zmienną ```string``` i przypisujemy jej wartość "Hello World". Następnie używamy metody ```length```, która zwraca nam długość ciągu znaków, czyli 11.

Możemy również wykorzystać pętlę ```for``` do iterowania po każdym znaku w ciągu i zwiększania licznika o 1, co będzie oznaczało długość ciągu.

```TypeScript
let string = "Hello World";
let counter = 0;

for (let i = 0; i < string.length; i++) {
  counter++;
}

console.log(counter);

// Output: 11
```

## Głębsze zagadnienia

### Kontekst historyczny

Wyliczanie długości ciągu znaków jest jedną z podstawowych operacji wykonywanych przez komputer. Pierwsze systemy operacyjne nie posiadały wbudowanej funkcji do tego celu i programiści musieli samodzielnie implementować tę funkcjonalność w swoich programach.

### Alternatywy

W języku TypeScript jest wiele wbudowanych metod pozwalających na manipulowanie ciągami znaków, takich jak ```substr```, ```substring``` czy ```slice```. Mogą one również zostać wykorzystane do określenia długości ciągu, jednak metoda ```length``` jest najprostszym i najczęściej używanym rozwiązaniem.

### Szczegóły implementacji

W języku TypeScript ciągi znaków przechowywane są jako tablice znaków, dlatego metoda ```length``` wykorzystuje właściwość ```length``` tablicy do określenia długości ciągu. W przypadku gdy ciąg jest pusty, metoda zwraca wartość 0.

## Zobacz także

Dokumentacja języka TypeScript: https://www.typescriptlang.org/docs/

Wprowadzenie do podstawowych operacji na ciągach w TypeScript: https://www.tutorialspoint.com/typescript/typescript_strings.htm