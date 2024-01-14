---
title:                "Go: Konwertowanie daty na ciąg znaków"
simple_title:         "Konwertowanie daty na ciąg znaków"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwertowanie daty na ciąg znaków jest nieodłączną częścią wielu aplikacji internetowych i mobilnych. Jest to niezbędne, aby móc wyświetlać dane datowe w czytelny sposób dla użytkowników. Pozwala również na sortowanie i filtrowanie danych chronologicznie. W tym artykule przyjrzymy się temu procesowi w języku Go i dowieśmy się, jak łatwo przekonwertować datę na ciąg znaków.

## Jak to zrobić

Aby przekonwertować datę na ciąg znaków w języku Go, pierwszym krokiem jest odpowiednie ustawienie formatu daty. W naszym przykładzie przyjmiemy format "dd/mm/yyyy". Następnie używamy funkcji `Time.Format()` do przekonwertowania daty na ciąg znaków. Przykładowy kod wyglądać będzie następująco:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	date := time.Date(2020, time.November, 3, 14, 30, 0, 0, time.UTC)
	formattedDate := date.Format("02/01/2006")
	fmt.Println(formattedDate)
}
```

Powyższy kod utworzy obiekt daty o wartości 3 listopada 2020 roku i przekonwertuje ją na ciąg znaków o formacie "dd/mm/yyyy". Wyjściem będzie "03/11/2020".

W języku Go możemy również korzystać z gotowych funkcji do konwersji daty na różne formaty, na przykład `Parse()` do zamiany ciągu znaków na datę. Poniżej przedstawiamy przykładowy kod z wykorzystaniem tej funkcji:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	dateString := "2020-11-03"
	date, _ := time.Parse("2006-01-02", dateString)
	fmt.Println(date)
}
```

Wyjściem będzie obiekt daty zgodny z podanym formatem.

## Głębsza analiza

Podczas konwertowania daty na ciąg znaków w języku Go ważne jest, aby odpowiednio ustawiać format. Możemy to zrobić używając znaków specjalnych, takich jak "02" dla dnia, "01" dla miesiąca i "2006" dla roku. Więcej informacji na temat znaków formatowania można znaleźć w dokumentacji języka Go.

Należy również pamiętać, że formatowanie daty może się różnić w zależności od lokalizacji. Na przykład daty w Stanach Zjednoczonych są zazwyczaj wyświetlane w formacie "mm/dd/yyyy", a nie "dd/mm/yyyy".

## Zobacz również

- Dokumentacja języka Go o funkcji `Time.Format()`: https://golang.org/pkg/time/#Time.Format
- Przykłady użycia funkcji `Parse()`: https://golang.org/pkg/time/#Parse
- Znaki specjalne do formatowania daty w języku Go: https://programming.guide/go/format-parse-string-time-date-example.html