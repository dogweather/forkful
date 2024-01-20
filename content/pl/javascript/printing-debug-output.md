---
title:                "Drukowanie komunikatów debugowania"
html_title:           "Haskell: Drukowanie komunikatów debugowania"
simple_title:         "Drukowanie komunikatów debugowania"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

---

## Co i dlaczego?
Drukowanie informacji diagnostycznych to sposób, w jaki programiści monitorują i debugują swój kod. Pomaga to zrozumieć, co dokładnie dzieje się "pod maską" podczas wykonywania programu.

## Jak to zrobić:
```Javascript
console.log("Witaj, jestem informacją diagnostyczną!");
```
Po uruchomieniu programu z powyższym poleceniem, wyświetli się komunikat: "Witaj, jestem informacją diagnostyczną!".

```Javascript
let zmiennaDebug = "Witaj ponownie";
console.debug(zmiennaDebug);
```
Podobnie jak wyżej, informacja diagnostyczna pokaże komunikat zapisany w zmiennej "zmiennaDebug": "Witaj ponownie".

## Bardziej szczegółowo:
Historia drukowania informacji diagnostycznych sięga początków programowania. Zostało to wprowadzone jako sposób na zrozumienie, co się dzieje z naszym kodem. Alternatywą dla `console.log` i `console.debug` jest `console.info` albo `console.warn`, które mogą być używane do drukowania różnych poziomów informacji diagnostycznych.

Kiedy użyjemy `console.log`, zostaje wywołana wewnętrzna metoda `process.stdout.write`, która wypisuje informacje do strumienia wyjściowego procesu. Przy `console.debug`, informacje zostaną wyświetlone tylko wtedy, gdy jest ustawione odpowiednie środowisko (np. `NODE_DEBUG`).

## Zobacz także:
3. [Metody obiektu konsoli](https://developer.mozilla.org/en-US/docs/Web/API/console)