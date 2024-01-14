---
title:    "Arduino: Przekształcanie ciągu znaków na małe litery"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

# Dlaczego

Jeśli programujesz w Arduino, być może zdarzyło Ci się napotkać potrzebę konwersji ciągu znaków na małe litery. Może chcesz porównać dwa tekstu lub po prostu chcesz, aby dane były ustandaryzowane. W tym artykule dowiesz się, jak to zrobić.

## Jak to zrobić

Aby przekonwertować ciąg znaków na małe litery w Arduino, można użyć funkcji toLowerCase(). Najpierw musisz określić zmienną przechowującą ciąg znaków, a następnie przypisać wynik funkcji toLowerCase() do tej samej zmiennej. Przykładowy kod można znaleźć poniżej.

```Arduino
String tekst = "HELLO";
tekst = tekst.toLowerCase();
Serial.println(tekst); // wyświetli "hello" w monitorze szeregowym
```

W przypadku, gdy chcesz zachować pierwotne wartości zmiennej i jedynie przekonwertować wyświetlane dane, można zastosować poniższy kod.

```Arduino
String tekst = "HELLO";
String tekst_malymi = tekst.toLowerCase();
Serial.println(tekst); // wyświetli "HELLO"
Serial.println(tekst_malymi); // wyświetli "hello"
```

## Głębszy przegląd

W przypadku konwersji na małe litery, nie ma konieczności używania funkcji toLowerCase() w Arduino. Można również skorzystać z funkcji toUpperCase() w celu przekonwertowania ciągu na duże litery. Istnieją również funkcje toUppercase() i toLowerCase() dla danych typu char, które nie są obiektami String.

Pamiętaj, że przy porównywaniu ciągów znaków, wielkość liter ma znaczenie. Dlatego zawsze warto przekonwertować ciąg na jedną konkretną wielkość, aby uniknąć błędów.

# Zobacz również

- [Dokumentacja funkcji toLowerCase() w Arduino](https://www.arduino.cc/reference/en/language/functions/string/functions/tolowercase/)
- [Dokumentacja funkcji toUpperCase() w Arduino](https://www.arduino.cc/reference/en/language/functions/string/functions/touppercase/)
- [Porównywanie ciągów znaków w Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/equalsignorecase/)