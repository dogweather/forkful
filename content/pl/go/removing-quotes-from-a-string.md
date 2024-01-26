---
title:                "Usuwanie cudzysłowów z ciągu znaków"
date:                  2024-01-26T03:39:40.556859-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usuwanie cudzysłowów z ciągu znaków"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Usuwanie cudzysłowów ze stringa oznacza pozbycie się tych irytujących znaków pojedynczego lub podwójnego cudzysłowu owijających Twój właściwy tekst. Robimy to, aby oczyścić dane, zapobiec błędom parsowania lub przygotować tekst do dalszego przetwarzania bez dodatkowego balastu znaków cudzysłowu.

## Jak to zrobić:

Oto prosty sposób, aby pozbyć się cudzysłowów w Go:

```go
package main

import (
	"fmt"
	"strings"
)

func removeQuotes(s string) string {
	return strings.Trim(s, "'\"")
}

func main() {
	quotedString := "\"Hello, World!\""
	fmt.Println("Original:", quotedString)

	unquotedString := removeQuotes(quotedString)
	fmt.Println("Unquoted:", unquotedString)
}
```

Efekt będzie wyglądał tak, cudzysłowy znikają:

```
Original: "Hello, World!"
Unquoted: Hello, World!
```

## Szczegółowa analiza

W przeszłości, gdy formaty danych i ich wymiana nie były ustandaryzowane, cudzysłowy w stringach mogły powodować chaos. Nadal mogą, szczególnie w JSON lub kiedy wciskamy stringi do baz danych. Pakiet `strings` w Go jest załadowany funkcją `Trim`, która eliminuje nie tylko białe znaki, ale jakiekolwiek znaki, których nie lubisz.

Dlaczego nie Regex? Cóż, `Trim` jest szybszy do prostych zadań, ale jeśli twoje stringi grają w chowanego z cudzysłowami w dziwnych miejscach, regex może być Twoją ciężką artylerią:

```go
import "regexp"

func removeQuotesWithRegex(s string) string {
	re := regexp.MustCompile(`^["']|["']$`)
	return re.ReplaceAllString(s, "")
}
```

To jak wybór między nożycami a piłą łańcuchową; wybierz narzędzie odpowiednie do pracy.

## Zobacz również

Aby dowiedzieć się więcej o pakiecie `strings` i jego narzędziach:
- [Pakiet strings](https://pkg.go.dev/strings)

Aby posiąść moc wyrażeń regularnych w Go:
- [Pakiet regexp](https://pkg.go.dev/regexp)

Chcesz zgłębić filozofię przycinania stringów?
- [Metoda Trim](https://blog.golang.org/strings)