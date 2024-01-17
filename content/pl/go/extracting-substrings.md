---
title:                "Wycinanie podciągów"
html_title:           "Go: Wycinanie podciągów"
simple_title:         "Wycinanie podciągów"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/extracting-substrings.md"
---

{{< edit_this_page >}}

# Co i dlaczego?

Wyciąganie podciągów (tak zwanych substringów) to proces wyodrębniania fragmentów tekstu z większego ciągu znaków. Programiści często wykonują tę czynność, aby przetworzyć lub analizować dane zgodnie z określonymi warunkami lub wymaganiami.

# Jak to zrobić?

Aby wyodrębnić podcięcie w Go, użyjemy funkcji ```strings[x:y]```, gdzie x jest indeksem początkowym, a y jest indeksem końcowym naszego podciągu. Przykładowy kod mógłby wyglądać następująco:

```Go
package main

import "fmt"
import "strings"

func main() {
    text := "To jest tekst przykładowy"
    substring := text[8:13] // wyciągniemy podciąg "tekst"
    fmt.Println(substring) // wypisze "tekst"
}
```

# Głębsze zanurzenie

Historia wyodrębniania podciągów sięga powstania pierwszych języków programowania, takich jak COBOL i FORTRAN. Współcześnie, istnieje kilka alternatywnych metod wyciągania podciągów, takich jak funkcja ```Substring``` w języku C#.

W Go, funkcja ```strings[x:y]``` jest zaimplementowana za pomocą typu wbudowanego ```string```, który nie może być zmieniany. Dzięki temu, podciągi są traktowane jak niezmienniki i zapewniają bezpieczne operacje na tekście.

# Zobacz także

- [Dokumentacja języka Go](https://golang.org/doc/)
- [Porównanie funkcji wyciągającej podciągi w językach programowania](https://www.w3resource.com/CSharp/csharp-string-methods/substring.php)