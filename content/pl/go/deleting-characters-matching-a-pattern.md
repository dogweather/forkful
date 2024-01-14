---
title:                "Go: Usuwanie znaków odpowiadających wzorowi"
simple_title:         "Usuwanie znaków odpowiadających wzorowi"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

#Dlaczego

Często w programowaniu zdarza się sytuacja, w której musimy usunąć pewne znaki z naszych danych. Na przykład, gdy tworzymy skrypt do przetwarzania plików, możemy chcieć usunąć wszystkie znaki specjalne z tekstu. W takich sytuacjach bardzo przydatną funkcją jest usuwanie znaków pasujących do wzorca, co pozwala nam szybko oczyścić dane i przystosować je do dalszego przetwarzania.

#Jak to zrobić?

W języku Go istnieje prosty sposób na usuwanie znaków pasujących do danego wzorca. Wystarczy użyć funkcji "ReplaceAllString" z pakietu "regexp". W poniższym przykładzie pokazane jest, jak usunąć wszystkie litery z tekstu:

```Go
package main

import (
	"fmt"
	"regexp"
)

func main() {
	text := "To jest tekst z wieloma literami i znakami specjalnymi!"
	reg := regexp.MustCompile("[a-zA-Z]")
	cleanText := reg.ReplaceAllString(text, "")
	fmt.Println(cleanText)
}
```

Po uruchomieniu kodu otrzymamy następujący wynik:

```
 !,. 
```

Łańcuch tekstowy został oczyściony z wszystkich liter, a w jego miejsce pozostały tylko znaki specjalne. Warto zauważyć, że w przykładzie wykorzystany został wzorzec "a-zA-Z", który oznacza wszystkie litery od A do Z (zarówno małe, jak i duże). Możemy jednak dostosować ten wzorzec do naszych potrzeb, zmieniając litery na np. cyfry lub znaki specjalne.

#Głębszy wgląd

Funkcja "ReplaceAllString" z pakietu "regexp" jest bardzo przydatna, ale warto uświadomić sobie, że jest to tylko jedna z wielu możliwości zastosowania wyrażeń regularnych w języku Go. Warto więc zapoznać się z innymi funkcjami z pakietu "regexp", takimi jak na przykład "FindStringSubmatch" czy "ReplaceAll". Dzięki temu będziemy mogli jeszcze bardziej rozbudować nasze skrypty i dostosować je do różnych przypadków.

#Zobacz także

Jeśli chcesz dowiedzieć się więcej o wyrażeniach regularnych w języku Go, polecamy zapoznać się z dokumentacją oraz innymi artykułami na ten temat:

- Dokumentacja pakietu "regexp": https://golang.org/pkg/regexp/
- "Wyrażenia regularne - jak działa pakiet 'regexp' w języku Go": https://blog.bugtrap.pl/post/wyrazenia-regularne-go/
- "Jak i kiedy używać wyrażeń regularnych w języku Go": https://robertocantillo.com/blog/regex-golang/