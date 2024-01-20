---
title:                "Konwersja daty na ciąg znaków"
html_title:           "Clojure: Konwersja daty na ciąg znaków"
simple_title:         "Konwersja daty na ciąg znaków"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Konwersja danych w postaci daty do ciągu znaków pozwala na łatwiejszą manipulację i prezentację danych dla użytkowników. Programiści wykonują to przede wszystkim w celu wyświetlania daty w czytelnym formacie.

## Jak to zrobić:

W Go, możemy skonwertować datę do ciągu znaków, stosując wbudowany pakiet `time` oraz metodę `Format`.

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	t := time.Now()
	fmt.Println(t.Format("2006-01-02"))
}
```

Za wywołanie powyższego kodu dostaniemy bieżącą datę wyświetloną w formacie: `RRRR-MM-DD`.

## Głębsze spojrzenie

Konwersja daty do ciągu znaków jest czynnością powszechną w programowaniu. W historii, różne języki oferowały różne metody do osiągnięcia tego, na przykład `strftime` w języku C.

Jeśli chodzi o Go, użyliśmy metody `Format`, która jest dość unikalna ze względu na niekonwencjonalne formatowanie oparte na "magicznej" dacie `2006-01-02 15:04:05`.

Inne metody, takie jak `Unix`, mogą być używane do konwersji daty do ciągu znaków w formacie unix timestamp. Z kolei `RFC3339` pozwoli na uzyskanie daty w formacie określonym przez standard RFC3339.

## Zobacz również

* Dokumentacja Go na temat pakietu `time`: https://golang.org/pkg/time/
* Więcej o standardzie `RFC3339`: https://www.ietf.org/rfc/rfc3339.txt
* Porównanie różnych języków programowania w zakresie formatowania dat: https://programming.guide/worlds-most-copied-so-called-piece-of-code.html