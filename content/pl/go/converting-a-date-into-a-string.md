---
title:    "Go: Konwertowanie daty na ciąg znaków."
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Dlaczego

Konwersja daty na ciąg znaków jest ważnym procesem przy programowaniu w języku Go. Pozwala ona na wyświetlenie daty w czytelnej dla człowieka formie, co jest szczególnie przydatne w aplikacjach, gdzie wyświetlanie daty jest częścią interfejsu użytkownika.

# Jak to zrobić

Aby skonwertować datę na ciąg znaków w języku Go, wystarczy wykorzystać funkcję `Format()` z pakietu `time`. Przykładowy kod wyglądałby następująco:

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    // Utworzenie obiektu typu Time z aktualną datą i czasem
    t := time.Now()

    // Konwersja na ciąg znaków z wykorzystaniem odpowiedniego formatu
    result := t.Format("02-01-2006")

    // Wyświetlenie wyniku
    fmt.Println(result)
}
```

W powyższym przykładzie, użyliśmy formatu `02-01-2006`, który oznacza, że wyświetlona zostanie data w formacie `dzień-miesiąc-rok`. Jednak istnieje wiele innych formatów, które można wykorzystać w zależności od potrzeb. Pełną listę formatów można znaleźć w dokumentacji języka Go.

Po uruchomieniu powyższego kodu, otrzymamy następujący wynik:

```
15-04-2019
```

# Głębokie zanurzenie

Aby lepiej zrozumieć proces konwertowania daty w Go, warto wiedzieć, że funkcja `Format()` korzysta z metody `String()` zaimplementowanej w structurze `Time` w pakiecie `time`. Metoda ta wykorzystuje formatowanie typu tekstowego oparte na wyrażeniach regularnych.

Funkcja `Format()` zawiera również wbudowane formaty, takie jak: `ANSIC`, `UnixDate`, `RubyDate` etc, które mogą być wykorzystane bezpośrednio bez konieczności deklarowania oddzielnego formatu.

# Zobacz również

1. [Dokumentacja języka Go na temat konwersji daty](https://golang.org/pkg/time/#Time.Format)
2. [Kurs języka Go w języku polskim](https://www.golang.cafe/)