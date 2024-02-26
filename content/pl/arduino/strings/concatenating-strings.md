---
date: 2024-01-20 17:34:16.557369-07:00
description: "\u0141\u0105czenie \u0142a\u0144cuch\xF3w znak\xF3w, czyli konkatenacja,\
  \ to proces tworzenia jednego \u0142a\u0144cucha z dw\xF3ch lub wi\u0119cej istniej\u0105\
  cych. Programi\u015Bci robi\u0105 to, by sk\u0142ada\u0107\u2026"
lastmod: '2024-02-25T18:49:34.034990-07:00'
model: gpt-4-1106-preview
summary: "\u0141\u0105czenie \u0142a\u0144cuch\xF3w znak\xF3w, czyli konkatenacja,\
  \ to proces tworzenia jednego \u0142a\u0144cucha z dw\xF3ch lub wi\u0119cej istniej\u0105\
  cych. Programi\u015Bci robi\u0105 to, by sk\u0142ada\u0107\u2026"
title: "\u0141\u0105czenie \u0142a\u0144cuch\xF3w znak\xF3w"
---

{{< edit_this_page >}}

## Co i dlaczego?
Łączenie łańcuchów znaków, czyli konkatenacja, to proces tworzenia jednego łańcucha z dwóch lub więcej istniejących. Programiści robią to, by składać wiadomości, generować dane wyjściowe lub pracować z tekstami w programach.

## Jak to zrobić:
```Arduino
String greeting = "Cześć ";
String name = "Janek";
String message = greeting + name + "!";
Serial.begin(9600);
Serial.println(message); // Wyświetla: Cześć Janek!
```

Alternatywnie, przy użyciu metod dla obiektów String:
```Arduino
String greeting = "Cześć ";
greeting.concat("Janek");
greeting.concat("!");
Serial.begin(9600);
Serial.println(greeting); // Wyświetla: Cześć Janek!
```

Jeżeli przetwarzasz dane w pamięci podręcznej, unikaj String gdzie to możliwe:
```Arduino
char greeting[] = "Cześć ";
char name[] = "Janek";
char message[20];
strcpy(message, greeting);
strcat(message, name);
strcat(message, "!");
Serial.begin(9600);
Serial.println(message); // Wyświetla: Cześć Janek!
```

## Deep Dive
Konkatenacja łańcuchów znaków nie jest czymś nowym – od dziesięcioleci jest fundamentem języków programowania. W przypadku Arduino i innych ograniczonych zasobowo środowisk, ważne jest, by pamiętać o zarządzaniu pamięcią. Używanie typu `String` może prowadzić do fragmentacji pamięci dynamicznej, szczególnie w przypadku wielokrotnego łączenia. Alternatywą może być użycie `char` arrays z funkcjami `strcpy` i `strcat`, które są bardziej przewidywalne pod względem zużycia pamięci.

## Zobacz również
- [Arduino Reference: String Object](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [Arduino Reference: String Concatenation](https://www.arduino.cc/en/Tutorial/BuiltInExamples/StringAdditionOperator)
