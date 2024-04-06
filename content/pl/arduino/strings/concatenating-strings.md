---
date: 2024-01-20 17:34:16.557369-07:00
description: "Jak to zrobi\u0107: Alternatywnie, przy u\u017Cyciu metod dla obiekt\xF3\
  w String."
lastmod: '2024-04-05T21:53:37.088922-06:00'
model: gpt-4-1106-preview
summary: "Alternatywnie, przy u\u017Cyciu metod dla obiekt\xF3w String."
title: "\u0141\u0105czenie \u0142a\u0144cuch\xF3w znak\xF3w"
weight: 3
---

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
