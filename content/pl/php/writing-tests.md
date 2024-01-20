---
title:                "Pisanie testów"
html_title:           "PHP: Pisanie testów"
simple_title:         "Pisanie testów"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/writing-tests.md"
---

{{< edit_this_page >}}

## Co to jest i dlaczego to robić?
Pisanie testów jest procesem, w którym programista tworzy małe programy, aby sprawdzić, czy ich kod działa zgodnie z oczekiwaniami. Jest to ważna część procesu tworzenia oprogramowania, ponieważ pozwala uniknąć błędów i zapewnić, że aplikacja działa poprawnie.

## Jak to zrobić:
```PHP
// Przykładowy test sprawdzający, czy suma dwóch liczb jest poprawna
function testSum() {
  $result = sum(2, 3);
  if ($result == 5) {
    echo "Test przeszedł pomyślnie";
  } else {
    echo "Test nie przeszedł";
  }
}

// Wywołanie funkcji testującej
testSum();
```

## Deep Dive:
Pisanie testów jest częścią metodyki TDD (Test Driven Development), która została wprowadzona w 2003 roku przez programistę Kenta Becka. Jest to podejście, w którym testy są pisane przed napisaniem właściwego kodu. Istnieją również inne metody tworzenia testów, takie jak BDD (Behavior Driven Development), gdzie testy są pisane w języku naturalnym, aby lepiej odzwierciedlić oczekiwane zachowanie aplikacji.

## Zobacz również: