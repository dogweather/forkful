---
title:                "Interpolacja łańcuchów znaków"
date:                  2024-01-20T17:51:09.288179-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolacja łańcuchów znaków"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Interpolacja ciągów znaków pozwala na wstawienie wartości zmiennych bezpośrednio wewnątrz stringów. Programiści używają tej techniki, by dynamicznie budować tekst i wzory, łącząc sztywny tekst z zmiennymi danymi.

## Jak to zrobić:
```go
package main

import (
	"fmt"
)

func main() {
	// Podstawowa interpolacja przy użyciu Sprintf
	name := "Anna"
	age := 28
	message := fmt.Sprintf("Cześć, mam na imię %s i mam %d lat.", name, age)
	fmt.Println(message)
	
	// Interpolacja z użyciem złożonych struktur
	type User struct {
		Name string
		Age  int
	}
	user := User{"Tomasz", 35}
	userInfo := fmt.Sprintf("Użytkownik %s ma %d lat.", user.Name, user.Age)
	fmt.Println(userInfo)
}
```

Sample output:
```
Cześć, mam na imię Anna i mam 28 lat.
Użytkownik Tomasz ma 35 lat.
```

## Głębiej w temat
Interpolacja ciągów znaków w języku Go jest przeważnie realizowana za pomocą funkcji `fmt.Sprintf`. Mechanizm ten ma swoje korzenie w językach takich jak C, które oferowały formatowanie tekstu z użyciem funkcji `sprintf`. W Go, dzięki formatowaniu z użyciem werb (takich jak `%s` dla stringów i `%d` dla liczb), możemy dokładnie określić jak dane powinny być wypisane.

Alternatywne metody interpolacji to sklejanie ciągów poprzez operator `+` lub stosowanie buforów i builderów w ciężkich operacjach tekstowych. W wersjach Go przed 1.10, budowanie długich ciągów za pomocą `+` było mniej wydajne niż teraz, ponieważ każde połączenie tworzyło nowy ciąg znaków.

Szczegół implementacyjny w Go to fakt, że `fmt.Sprintf` korzysta z odzwierciedlenia (reflection), co może wpłynąć na wydajność. Dlatego programiści czasem decydują się na inne metody w krytycznych dla wydajności przypadkach.

## Zobacz również
- Dokumentacja `fmt` pakietu: https://golang.org/pkg/fmt/
- Go by Example - String Formatting: https://gobyexample.com/string-formatting
- The Go Blog - Strings: https://blog.golang.org/strings
