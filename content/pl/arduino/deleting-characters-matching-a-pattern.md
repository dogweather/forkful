---
title:                "Usuwanie znaków pasujących do wzorca"
html_title:           "C: Usuwanie znaków pasujących do wzorca"
simple_title:         "Usuwanie znaków pasujących do wzorca"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Usuwanie znaków według wzorca to operacja polegająca na zidentyfikowaniu i usunięciu określonych znaków z tekstu w procesie programowania. Programiści robią to, aby przesiewać i oczyszczać dane wejściowe, usuwać niepotrzebne spacje, znaki specjalne i formatowanie nim przystąpią do przetwarzania danych.

## Jak to zrobić:

Poniżej znajduje się przykład usuwania znaków pasujących do wzorca w Arduino.

```Arduino
String inputStr = "Przykładowy tekst.";
String outputStr;
for (char &c: inputStr)
{
  if (c != '.') // wzorzec, którego szukamy
  {
    outputStr += c;  
  }
}
Serial.println(outputStr);
```
Po uruchomieniu kodu, na konsoli powinniśmy zobaczyć "Przykładowy tekst", gdzie został usunięty znak ".".

## Pogłębienie:

Usuwanie znaków według wzorca nie jest nowym konceptem w programowaniu. Kiedy dane wejściowe są nieprzewidywalne, często musimy je oczyścić z niepotrzebnych znaków. Inna strategia to korzystanie z funkcji `replace()`. Ta metoda jednak wymaga więcej zasobów, ponieważ tworzy nowy łańcuch zamiast modyfikować istniejący.

## Zobacz także:

Jeśli chcesz dowiedzieć się więcej o programowaniu w Arduino i szukaniu wzorców, możesz odwiedzić następujące źródła:

1. [Dokumentacja Arduino](https://www.arduino.cc/reference/pl/)
2. [Poradnik do operacji na ciągach znaków](https://startingelectronics.org/software/arduino/learn-to-program-course/18-string-functions/)
3. [Dokumentacja funkcji replace()](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replace/)