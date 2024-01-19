---
title:                "Pobieranie aktualnej daty"
html_title:           "Arduino: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Pobieranie bieżącej daty polega na wydobyciu aktualnych informacji na temat dnia, miesiąca i roku z systemu komputera. Programiści robią to w różnych celach, takich jak logowanie, timelining czy porównanie dat. 

## Jak to zrobić:

Oto przykład, jak możemy uzyskać bieżącą datę w języku Go:

```Go
package main
import (
	"fmt"
	"time"
)

func main() {
	currentTime := time.Now()
	fmt.Println("Bieżąca Data :", currentTime.Format("2006-01-02"))
}
```

Po uruchomieniu powyższego kodu, otrzymamy output:

```
Bieżąca Data : 2021-12-15
```

## Deep Dive:

1. *Kontekst historyczny*: Biblioteka `time` w Go była częścią języka od pierwszej stabilnej wersji, opublikowanej w 2007 roku. Została zoptymalizowana na przestrzeni lat, aby obsługiwać wiele różnych formatów daty i czasu.
2. *Alternatywy*: Możemy też użyć pakietu `time` do pobierania innych informacji, takich jak godzina, minuta, sekunda, dzień tygodnia itp. Jednakże, do bardziej szczegółowych zadaniań, jak manipulacja strefami czasowymi, możemy użyć pakietu `time/tzdata`.
3. *Szczegóły implementacji*: Funkcja `Now()` z pakietu `time` zwraca bieżący lokalny czas. Metoda `Format` pozwala na formatowanie czasu do dowolnego pożądanego wyglądu.

## Zobacz też:

1. Dokumentacja Go na temat pakietu `time`: [Kliknij tutaj](https://golang.org/pkg/time/)
2. Artykuł na temat formatowania czasu w Go: [Kliknij tutaj](https://yourbasic.org/golang/format-parse-string-time-date-example/)
3. Tutorial na temat manipulacji datą i czasem w Go: [Kliknij tutaj](https://tutorialedge.net/golang/working-with-dates-golang/)