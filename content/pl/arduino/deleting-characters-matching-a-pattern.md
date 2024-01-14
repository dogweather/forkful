---
title:                "Arduino: Usuwanie znaków pasujących do wzorca"
simple_title:         "Usuwanie znaków pasujących do wzorca"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy zdarzyło Ci się kiedykolwiek potrzebować usunąć wybrane znaki z tekstu w Twoim programie Arduino? Może być to przydatne, jeśli chcesz usunąć znaki specjalne lub białe znaki z otrzymanego ciągu. W tym artykule dowiesz się jak prosto usunąć znaki pasujące do określonego wzoru, za pomocą prostego kodu w Arduino.

## Jak to zrobić

Aby usunąć znaki pasujące do wzoru, będziemy korzystać z funkcji `replace()` dostępnej w bibliotece standardowej C++. Ta funkcja pozwala nam zastąpić wybrane znaki lub wzorce w tekście. Sprawdźmy jak to działa w praktyce:

```Arduino
String text = "Przykładowy tekst z *zakazanymi* znakami";
String pattern = "*";
String replacement = "";
text.replace(pattern, replacement);
Serial.println(text);
```
**Output:**

```Arduino
Przykładowy tekst z zakazanymi znakami
```

W powyższym przykładzie przekazujemy funkcji `replace()` trzy parametry: wzorzec, który chcemy usunąć (`*`), pustą zmienną na miejsce usuniętego wzorca (`""`) oraz zmienną zawierającą tekst, w którym chcemy dokonać zmian.

## Deep Dive

Funkcja `replace()` może być również używana do zastępowania wybranego tekstu innym tekstem. Na przykład:

```Arduino
String text = "Oryginalny tekst: ABCDEF";
String pattern = "ABC";
String replacement = "123";
text.replace(pattern, replacement);
Serial.println(text);
```

**Output:**

```Arduino
Oryginalny tekst: 123DEF
```

Funkcja `replace()` jest wygodnym sposobem na manipulowanie tekstem w programie Arduino. Pamiętaj jednak, że zastąpi tylko pierwsze wystąpienie wzorca. Jeśli chcesz zastąpić wszystkie wystąpienia, musisz użyć pętli `while` lub dodatkowej funkcji `replaceAll()` dostępnej w niektórych bibliotekach.

## Zobacz także

- Dokumentacja funkcji `replace()`: [https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replace/](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replace/)
- Przykładowa biblioteka `StringReplace`: [https://github.com/esp8266/Arduino/tree/master/libraries/String/src](https://github.com/esp8266/Arduino/tree/master/libraries/String/src)