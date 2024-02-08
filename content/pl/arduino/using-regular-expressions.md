---
title:                "Korzystanie z wyrażeń regularnych"
aliases:
- pl/arduino/using-regular-expressions.md
date:                  2024-02-03T19:16:10.427019-07:00
model:                 gpt-4-0125-preview
simple_title:         "Korzystanie z wyrażeń regularnych"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?
Wyrażenia regularne (regex) to sekwencje znaków definiujące wzorce wyszukiwania, głównie używane do dopasowywania i manipulacji ciągami znaków. Programiści wykorzystują regex w projektach Arduino do parsowania danych wejściowych z portu szeregowego, walidacji danych wprowadzanych przez użytkownika, czy ekstrakcji danych z ciągów tekstowych, zwiększając efektywność i elastyczność przetwarzania danych.

## Jak to zrobić:
Arduino nie posiada wbudowanego wsparcia dla regex bezpośrednio w swojej standardowej bibliotece. Jednakże, możesz osiągnąć funkcjonalność podobną do regex dla prostych wzorców, używając podstawowych funkcji operujących na ciągach znaków, lub, dla bardziej złożonych potrzeb, zintegrować bibliotekę stron trzecich taką jak `regex`.

### Podstawowe dopasowywanie ciągów bez Regex
Dla podstawowych potrzeb, takich jak znalezienie podciągu, możesz użyć funkcji `String.indexOf()`:
```cpp
String data = "Wartość czujnika: 12345";
int index = data.indexOf("wartość:");
if (index != -1) {
  String value = data.substring(index + 7).trim();
  Serial.println(value); // Wyświetla: 12345
}
```

### Użycie biblioteki stron trzecich dla Regex
Do obsługi bardziej złożonych wzorców, warto rozważyć użycie biblioteki takiej jak `regex`. Po zainstalowaniu biblioteki, możesz użyć jej w następujący sposób:

1. **Instalacja**: Biblioteka `regex` może nie być bezpośrednio dostępna w Menedżerze Bibliotek Arduino, więc może być konieczne ręczne zainstalowanie jej przez pobranie z zaufanego źródła i dodanie do folderu bibliotek Arduino.

2. **Przykład użycia**:
Zakładając, że biblioteka oferuje funkcjonalności podobne do standardowych implementacji regex, możesz użyć jej w następujący sposób:

```cpp
#include <regex.h>

void setup() {
  Serial.begin(9600);
  while (!Serial); // Czekaj na gotowość Serial
  
  regex_t reg;
  const char* pattern = "[0-9]+"; // Dopasowuje sekwencję cyfr
  regcomp(&reg, pattern, REG_EXTENDED);
  
  const char* test_str = "Wartość czujnika: 12345";
  
  regmatch_t matches[1];
  if (regexec(&reg, test_str, 1, matches, 0) == 0) {
    // Wyodrębnij i wydrukuj pasujący fragment
    int start = matches[0].rm_so;
    int end = matches[0].rm_eo;
    char match[end-start+1];
    strncpy(match, test_str + start, end-start);
    match[end-start] = '\0';
    
    Serial.print("Znaleziono dopasowanie: ");
    Serial.println(match); // Wyświetla: 12345
  } else {
    Serial.println("Nie znaleziono dopasowania");
  }
  
  regfree(&reg); // Zwolnij przydzieloną pamięć dla regex
}

void loop() {
  // umieść tutaj swój główny kod, aby był wykonywany wielokrotnie:
}
```

**Uwaga**: Składnia i konkretne funkcje użyte tutaj mają charakter ilustracyjny i mogą różnić się w zależności od szczegółów implementacji biblioteki `regex`, którą wybierzesz. Zawsze odwołuj się do dokumentacji biblioteki, aby uzyskać dokładne i aktualne informacje.
