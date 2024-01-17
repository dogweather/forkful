---
title:                "Zmiana na wielką literę ciągu znaków"
html_title:           "Arduino: Zmiana na wielką literę ciągu znaków"
simple_title:         "Zmiana na wielką literę ciągu znaków"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Z wielu powodów programiści chcą czasami zmienić niektóre litery w ciągu znaków na duże. Proces ten nazywa się wydobywaniem (ang. capitalization). Pozwala to na lepszą czytelność kodu i ułatwia jego interpretację przez komputer.

## Jak to zrobić?
Oto przykład kodu w języku Arduino, który wydoba podany ciąg znaków:
```Arduino
String ciag = "witaj swiecie";
Serial.println(ciag.toUpperCase());
```
Powyższy kod wypisze na monitorze szeregujnym (ang. serial monitor) ciąg "WITAJ SWIECIE".

## Głębsza analiza
Historia wydobywania sięga czasów maszyn telegrafiocode.wzór (ang. Morse code). Wówczas było to potrzebne ze względu na ograniczenia techniczne urządzeń i transmisję sygnałów. Obecnie istnieją również inne metody zmiany wielkości liter w ciągach znaków, takie jak konwersja na format ASCII czy użycie pętli do iteracji po każdym znaku.

## Zobacz również
Jeśli chcesz dowiedzieć się więcej o wydobywaniu, możesz przeczytać o tym w dokumentacji języka Arduino lub poszukać tutoriali w Internecie.