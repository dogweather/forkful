---
title:                "Łączenie ciągów znaków"
html_title:           "Arduino: Łączenie ciągów znaków"
simple_title:         "Łączenie ciągów znaków"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## Co to i dlaczego?

Konkatenacja stringów polega na łączeniu dwóch lub więcej ciągów tekstów w jedną całość. Programiści robią to, aby skrócić kod i zwiększyć czytelność.

## Jak to zrobić:

W Arduino możemy dokonać konkatenacji za pomocą operatora '+'. Poniżej znajduje się przykład:
```Arduino
String firstString = "Arduino";
String secondString = " Programming";
String finalString = firstString + secondString;
Serial.println(finalString);
```
Po wywołaniu powyższego kodu, otrzymujemy następującą wiadomość:
```Arduino
"Arduino Programming"
```
## Dogłębna analiza:

Konkatenacja ciągów jest fundamentalnym elementem programowania, istniejącym od dawna. Alternatywą dla używania operatora '+' jest użycie funkcji `concat()` dla bardziej skomplikowanych operacji.

Pamiętaj jednak, że konkatenacja w Arduino jest złożonym procesem. Arduino nie obsługuje dynamicznej alokacji pamięci, więc dodawanie tekstów komplikuje zarządzanie pamięcią.

## Zobacz też:

1. [Arduino String concat()](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/concat/)
3. [Arduino String Reference](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)