---
title:                "Wycinanie podciągów"
html_title:           "Arduino: Wycinanie podciągów"
simple_title:         "Wycinanie podciągów"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?
Wyodrębnianie podciągów to proces, w którym programista wybiera i wydobywa określony fragment tekstu lub danych z większego ciągu znaków lub danych. Jest to często wykorzystywane w programowaniu do analizowania i przetwarzania danych tekstowych lub ciągów znaków w celu uzyskania potrzebnych informacji. 

## Jak to zrobić:
Aby wyodrębnić podciąg w Arduino, użyj funkcji substring () i podaj odpowiednie parametry. Funkcja ta zwróci wybrane znaki z określonego łańcucha. Przykładowy kod może wyglądać następująco:

```Arduino
String nazwa = "Arduino"; // zadany ciąg znaków
int poczatek = 2;  // indeks początkowy podciągu (licząc od 0)
int dlugosc = 4; // długość podciągu
String podciag = nazwa.substring(poczatek, poczatek+dlugosc); // użycie funkcji substring
```

W powyższym przykładzie wybrany zostanie podciąg "duin". 

## Zagłębie:
Funkcja substring () jest często używana w różnych językach programowania, w tym w Arduino. Alternatywnym sposobem wyodrębniania podciągów może być użycie pętli i funkcji do porównywania i analizowania znaków w ciągach tekstowych. Implementacja funkcji substring () w Arduino wykorzystuje wskaźnik do wybrania odpowiednich znaków z ciągu. 

## Zobacz też:
- [Dokumentacja Arduino o funkcji substring ()](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/)
- [Wykorzystanie funkcji substring () w programowaniu](https://www.programiz.com/java-programming/library/string/substring)