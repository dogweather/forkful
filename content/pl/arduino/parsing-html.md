---
title:                "Przetwarzanie HTML"
date:                  2024-01-20T15:30:08.963432-07:00
html_title:           "Bash: Przetwarzanie HTML"
simple_title:         "Przetwarzanie HTML"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)

Parsing HTML to sposób, by komputer czytał i rozumiał struktury HTML - język budowy stron internetowych. Programiści parsują HTML, żeby wydobyć dane, zarządzać treścią lub naprawić błędy.

## How to: (Jak to zrobić:)

Obecnie nie ma dedykowanej funkcji do parsowania HTML w Arduino. Można jednak użyć biblioteki String, by przetworzyć prosty HTML.

```Arduino
#include <String.h>

void setup() {
  Serial.begin(9600);
  String htmlData = "<html><body><h1>Arduino Rocks!</h1></body></html>";
  int startIndex = htmlData.indexOf("<h1>") + 4;
  int endIndex = htmlData.indexOf("</h1>");
  String heading = htmlData.substring(startIndex, endIndex);
  Serial.println(heading);
}

void loop() {
  // nic nie robimy w pętli
}
```

Sample output:
```
Arduino Rocks!
```

## Deep Dive (W głąb tematu):

Arduino nie ma wbudowanej obsługi XML czy HTML, więc operujemy na prostych stringach. W przeszłości HTML interpretowano ręcznie lub przy użyciu bardziej skomplikowanych bibliotek na innych platformach. Alternatywy to używanie wyrażeń regularnych lub dedykowanych bibliotek jak Gumbo-parser dla C++. Ważne, by pamiętać, że Arduino ma ograniczone zasoby i nie nadaje się do skomplikowanego parsowania.

## See Also (Zobacz też):

- Dokumentacja String w Arduino: https://www.arduino.cc/reference/en/language/variables/data-types/string/
- Przykłady użycia wyrażeń regularnych: https://www.regular-expressions.info/
- Gumbo-parser, parser HTML dla C++: https://github.com/google/gumbo-parser