---
title:                "Arduino: Konwertowanie ciągu znaków na małe litery"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Często w projektach związanych z programowaniem mikrokontrolerów Arduino musimy operować na tekście. Dlatego warto poznać jak efektywnie zamieniać litery w ciągu znaków na małe litery.

## Jak to zrobić?

Aby zamienić ciąg znaków na małe litery w Arduino, możemy użyć funkcji `toLowerCase()`. Poniżej znajduje się przykładowy kod, który zamienia ciąg `Hello World!` na `hello world!`.

```Arduino
String str = "Hello World!";
String lower_str = str.toLowerCase();
Serial.println(lower_str);
```

Output: `hello world!`

## Głębsza analiza

Funkcja `toLowerCase()` wykonuje pętlę po każdym znaku w ciągu i zamienia go na odpowiadającą mu małą literę, jeśli jest to możliwe. Dlatego ważne jest, aby upewnić się, że używany przez nas ciąg znaków jest zbudowany tylko z liter. W przypadku użycia innych znaków (np. cyfr lub symboli), funkcja pozostawi je bez zmian.

## Zobacz także

- [Dokumentacja funkcji toLowerCase() w Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/tolowercase/)
- [Przykładowy projekt Arduino z użyciem funkcji toLowerCase()](https://create.arduino.cc/projecthub/circuito-io-team/text-transformation-with-arduino-to-lower-case-7b81ec)