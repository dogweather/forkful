---
title:    "Arduino: Wyszukiwanie i zastępowanie tekstu"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Dlaczego

Często zdarza się, że w programowaniu trzeba dokonywać pewnych zmian w tekście, takich jak zamiana jednego słowa na inne lub usunięcie pewnych znaków. Występuje to szczególnie często w przypadku programowania na Arduino. Aby oszczędzić czas i zmniejszyć szansę na popełnienie błędów, warto zapoznać się z techniką wyszukiwania i zamiany tekstu.

## Jak to zrobić

Do wyszukiwania i zamiany tekstu w kodzie Arduino można wykorzystać funkcję `replace()`. Przykładowy kod wyglądałby następująco:

```Arduino
String tekst = "Witaj Arduino!";
String zmienionyTekst = tekst.replace("Witaj", "Cześć");
Serial.println(zmienionyTekst);
```

W powyższym przykładzie, wykorzystując funkcję `replace()`, zamieniono słowo "Witaj" na "Cześć" w zmiennej `tekst`. Następnie, za pomocą funkcji `println()`, zmieniony tekst zostaje wyświetlony na monitorze szeregowym.

Dodatkowo, w celu wyszukiwania i zamiany tekstu w całym programie, można wykorzystać funkcję `replace()` w pętli `while`. Przykładowy kod wyglądałby tak:

```Arduino
String text = "Hello World";
text.replace("Hello", "Hi");
Serial.println(text);
```

W powyższym przykładzie, za pomocą pętli `while` słowo "Hello" zostaje zamienione na "Hi" w zmiennej `text`. Następnie, za pomocą funkcji `println()`, zmieniony tekst zostaje wyświetlony na monitorze szeregowym.

## Pogłębiona analiza

Powyższe przykłady pokazują, że za pomocą funkcji `replace()` można łatwo wyszukiwać i zamieniać tekst w programie Arduino. Jednak warto zauważyć, że funkcja ta jest wrażliwa na wielkość liter - słowo musi zostać wprowadzone tak, jak występuje w zmiennej. W celu uniknięcia błędów, można wykorzystać funkcję `toLowerCase()` lub `toUpperCase()` odpowiadającą za zmianę wielkości liter.

Dodatkowo, funkcja `replace()` może również przyjmować zmienne w miejsce stałych wartości. Dzięki temu można dynamicznie zmieniać tekst w zależności od wartości zmiennych w programie.

## Zobacz także

Więcej informacji na temat funkcji `replace()` można znaleźć na stronie dokumentacji Arduino: [http://arduino.cc/reference/en/language/variables/data-types/string/functions/replace/](http://arduino.cc/reference/en/language/variables/data-types/string/functions/replace/)

Inne przydatne funkcje pracy z tekstem na Arduino można znaleźć na stronie: [http://arduino.cc/reference/en/language/functions/communication/serial/println/](http://arduino.cc/reference/en/language/functions/communication/serial/println/)