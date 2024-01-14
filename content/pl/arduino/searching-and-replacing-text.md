---
title:                "Arduino: Wyszukiwanie i zastępowanie tekstu"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

Na czym polega programowanie Arduino:

## Dlaczego

Programowanie Arduino to fascynujący świat, który pozwala nam tworzyć różnego rodzaju projekty z wykorzystaniem mikrokontrolera. Jednym z najważniejszych aspektów programowania jest praca z tekstem. Dlatego dzisiaj skupimy się na temacie wyszukiwania i zastępowania tekstu w kodzie Arduino.

## Jak to zrobić

Wyszukiwanie i zastępowanie tekstu odgrywa ważną rolę przy pracy z kodem, ponieważ pozwala nam szybko i łatwo wprowadzać zmiany w naszym programie. Aby przeprowadzić tę operację w Arduino, musimy użyć funkcji `replace()` i `indexOf()`.

```Arduino
String napis = "Witaj świecie!";
int pozycja = napis.indexOf("świat");
napis.replace(pozycja, 6, "nieznajomy");
Serial.println(napis);
```

Powyższy kod najpierw znajduje indeks wystąpienia słowa "świat" w zmiennej `napis`, a następnie zmienia go na słowo "nieznajomy". W efekcie na naszym serial monitorze wyświetli się napis "Witaj nieznajomy!".

Podobnie, jeśli chcielibyśmy zmienić wiele wystąpień słowa w tekście, możemy użyć pętli `while` lub `for` w połączeniu z funkcjami `indexOf()` i `replace()`.

## Wnikliwe omówienie

Podczas wyszukiwania i zastępowania tekstu, ważne jest zwrócenie uwagi na przyjęty format danych. Funkcja `replace()` wymaga podania początkowej pozycji tekstu, który chcemy zmienić, długości tekst, który chcemy zmienić oraz nowego tekstu.

Dla zaawansowanych użytkowników Arduino, istnieje możliwość wykorzystania bibliotek do pracy z wyrażeniami regularnymi, które pozwalają na bardziej skomplikowane wyszukiwania i zastępowania tekstu. Jedną z takich bibliotek jest na przykład `Regex.h`.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o programowaniu Arduino, polecamy przeczytać nasz wcześniejszy post na temat obsługi wejść i wyjść w Arduino. Możesz również zapoznać się z innymi artykułami ze strony `arduino.org` lub dołączyć do grupy dyskusyjnej na Facebooku, gdzie możesz wymieniać się pomysłami z innymi miłośnikami Arduino.