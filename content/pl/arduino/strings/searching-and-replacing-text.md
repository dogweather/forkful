---
date: 2024-01-20 17:57:29.944599-07:00
description: "How to: (Jak to zrobi\u0107?) W \u015Brodowisku Arduino szukanie i zamiana\
  \ tekstu nie jest funkcj\u0105 dost\u0119pn\u0105 \"od r\u0119ki\", ale mo\u017C\
  esz to osi\u0105gn\u0105\u0107, u\u017Cywaj\u0105c prostych\u2026"
lastmod: '2024-04-05T21:53:37.082555-06:00'
model: gpt-4-1106-preview
summary: "(Jak to zrobi\u0107?) W \u015Brodowisku Arduino szukanie i zamiana tekstu\
  \ nie jest funkcj\u0105 dost\u0119pn\u0105 \"od r\u0119ki\", ale mo\u017Cesz to\
  \ osi\u0105gn\u0105\u0107, u\u017Cywaj\u0105c prostych funkcji."
title: Wyszukiwanie i zamiana tekstu
weight: 10
---

## How to: (Jak to zrobić?)
W środowisku Arduino szukanie i zamiana tekstu nie jest funkcją dostępną "od ręki", ale możesz to osiągnąć, używając prostych funkcji. Przykład poniżej pokazuje jak znaleźć i zmienić słowo w Stringu.

```Arduino
String replaceText(String source, String toReplace, String replaceWith) {
    int startIndex = source.indexOf(toReplace);
    if (startIndex == -1) {
        return source; // Nie znaleziono tekstu
    }
    return source.substring(0, startIndex) + replaceWith + source.substring(startIndex + toReplace.length());
}

void setup() {
    Serial.begin(9600);
    String original = "Cześć Arduino";
    String modified = replaceText(original, "Cześć", "Witaj");
    Serial.println(modified); // Wyświetli "Witaj Arduino"
}

void loop() {
    // Pętla nie jest potrzebna dla tego przykładu
}
```

## Deep Dive (Głębsze spojrzenie)
Szukanie i zamiana tekstu ma swoje korzenie w edycji tekstu i przetwarzaniu danych. W systemach operacyjnych i bardziej zaawansowanych językach programowania, takich jak Python, funkcjonalności te są wbudowane i znacznie rozbudowane. W Arduino brakuje natywnego wsparcia, co stawia przed nami wyzwanie implementacji własnych funkcji.

Alternatywą dla `String` w Arduino jest używanie klasycznych tablic `char` i funkcji `strtok`, `strncpy` itp., które pochodzą z języka C. Choć są one bardziej złożone w użyciu i mniej bezpieczne pod względem operacji na pamięci, pozwalają na oszczędność zasobów, co jest kluczowe w embedded systems.

## See Also (Zobacz również)
- [Arduino String Reference](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [C++ Reference for std::string](https://en.cppreference.com/w/cpp/string/basic_string)
- [Arduino Forum](https://forum.arduino.cc/): Znajdź dyskusje na temat manipulacji tekstami i Stringów w Arduino.

Pamiętaj, że operacje na Stringach w Arduino mogą prowadzić do fragmentacji pamięci, dlatego warto zapoznać się z alternatywami i wskazówkami dotyczącymi optymalizacji.
