---
title:                "Analiza daty z ciągu znaków"
html_title:           "Arduino: Analiza daty z ciągu znaków"
simple_title:         "Analiza daty z ciągu znaków"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Co to jest parsowanie daty z ciągu znaków?

Parsowanie daty z ciągu znaków to proces odczytywania daty z tekstu i konwertowania jej do odpowiedniego formatu na potrzeby programowania. Programiści często muszą manipulować danymi w różnych formatach, w tym również datami, dlatego parsowanie jest ważnym narzędziem w programowaniu.

## Jak to zrobić:

Arduino oferuje wbudowaną funkcję `parseDate()`, która pozwala na przekształcenie daty z ciągu znaków do obiektu typu `Date`. Poniższy kod demonstruje wykorzystanie tej funkcji w celu przekształcenia daty ze zmiennej tekstowej `stringDate`:

```
String stringDate = "31/05/2021";
Date date = parseDate(stringDate);
Serial.println(date); // Wypisze "2021-05-31T00:00:00"
```

## Głębsza analiza:

Parsowanie daty z ciągu znaków jest procesem, który wyewoluował razem z rozwojem programowania. Wcześniej programiści musieli samodzielnie pisać funkcje do konwertowania dat, co było czasochłonne i potencjalnie podatne na błędy. Dzięki funkcjom wbudowanym, takim jak `parseDate()` w Arduino, ta czynność jest znacznie prostsza i szybsza. Istnieją również inne sposoby przekształcania daty, takie jak wykorzystanie bibliotek lub pisząc własne funkcje.

## Zobacz także:

Więcej informacji na temat parsowania daty w Arduino można znaleźć na stronie dokumentacji:
https://www.arduino.cc/reference/en/language/functions/communication/parseDate/

Można również zapoznać się z innymi sposobami na pracę z datami na stronie:
https://www.arduino.cc/reference/en/language/variables/data-types/date/