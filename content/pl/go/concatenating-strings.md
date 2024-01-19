---
title:                "Konkatenacja ciągów znaków"
html_title:           "Bash: Konkatenacja ciągów znaków"
simple_title:         "Konkatenacja ciągów znaków"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Konkatenacja stringów to proces łączenia dwóch lub więcej tekstów w jeden. Programiści korzystają z tego do manipulowania i formatowania danych tekstowych.

## Jak to Zrobić:
W Go, do konkatenacji stringów używamy operatora "+". Patrz na przykład poniżej:
```Go
package main
import "fmt"

func main() {
 str1 := "Cześć, "
 str2 := "świecie!"
 result := str1 + str2
 fmt.Println(result)
}
```
Gdy uruchomisz ten kod, zobaczysz następujące wyjście:
```Go
Cześć, świcie!
```
## Głębsza Wiedza
Historia konkatenacji wchodzi głęboko w korzenie programowania. Można to spotkać w prawie każdym języku programowania. W Go, spoza operatora "+", istnieje również funkcja `strings.Join()` i metoda `bytes.Buffer`.

Funkcja `strings.Join()` jest alternatywą, którą można wykorzystać gdy mamy więcej niż dwa stringi do połączenia. Przykład:
```Go
package main
import (
 "fmt"
 "strings"
)

func main() {
 str := []string{"Cześć, ", "świecie!"}
 result := strings.Join(str, "")
 fmt.Println(result)
}
```
Metoda `bytes.Buffer` jest znacznie wydajniejsza przy dużej ilości danych:
```Go
package main
import (
 "bytes"
 "fmt"
)

func main() {
 var buffer bytes.Buffer

 for i := 1; i <= 10; i++ {
  buffer.WriteString("string ")
 }

 fmt.Println(buffer.String())
}
```
## Zobacz Również
1. Dokumentacja Go dla pakietu string: https://golang.org/pkg/strings/
2. Dokumentacja Go dla pakietu bytes: https://golang.org/pkg/bytes/
3. Blog o konkatenacji stringów w Go: https://go.dev/blog/strings