---
title:                "Arduino: Wyszukiwanie i zamiana tekstu"
simple_title:         "Wyszukiwanie i zamiana tekstu"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

Wiele razy w trakcie programowania na platformie Arduino może być konieczne zmienienie pewnego tekstu lub wyrażenia w kodzie. Może to być spowodowane potrzebą aktualizacji, debugowania lub modyfikacji programu. Dlatego znajomość sposobu wyszukiwania i wymiany tekstu jest istotna dla każdego Arduino programisty.

## Jak To Zrobić

Wiele osób może zastanawiać się, jak skutecznie przeprowadzić operację wyszukiwania i wymiany tekstu w kodzie programu Arduino. Poniżej przedstawione są przykłady kodu wraz z odpowiednimi wyjściami, które pomogą w zrozumieniu oraz wykorzystaniu tej funkcjonalności.

```Arduino
// Przykładowy kod z wykorzystaniem funkcji replace()

#include <String.h>

String tekst = "Dzień dobry, Arduino!";

void setup() {
  // Drukowanie tekstu przed zmianą
  Serial.println("Tekst przed zmianą:");
  Serial.println(tekst);

  // Wymiana tekstu "dobry" na "wieczór"
  tekst.replace("dobry", "wieczór");

  // Drukowanie tekstu po zmianie
  Serial.println("Tekst po zmianie:");
  Serial.println(tekst);
}

void loop() {
  // pusta pętla
}
```

Wyjście:

```
Tekst przed zmianą:
Dzień dobry, Arduino!
Tekst po zmianie:
Dzień wieczór, Arduino!
```

## Zagłębienie

Funkcja replace() użyta w powyższym przykładzie jest jednym z kilku sposobów na wyszukiwanie i wymianę tekstu w kodzie Arduino. Istnieje również wiele innych funkcji i bibliotek, które umożliwiają wykonanie tej operacji w inny sposób.

Na przykład, w bibliotece <string.h> dostępna jest funkcja strcpy() pozwalająca na kopiowanie tekstu do nowej zmiennej oraz funkcja strcat() służąca do "łączenia" tekstów. Można również wykorzystać instrukcje warunkowe, aby wykonywać różne czynności w zależności od znalezionego tekstu.

Znajomość tych różnych sposobów wyszukiwania i wymiany tekstu w kodzie Arduino może pomóc w rozwiązywaniu różnego rodzaju problemów i ułatwić modyfikację programu w przyszłości.

## Zobacz również

- Funkcja replace() w bibliotece Arduino:
https://www.arduino.cc/reference/en/language/functions/string-functions/replace/
- Wyszukiwanie i wymiana tekstu w programie Arduino:
https://www.instructables.com/id/Find-Replace-Arduino/
- Instrukcje warunkowe w Arduino:
https://www.arduino.cc/reference/en/language/structure/control-structure/if/