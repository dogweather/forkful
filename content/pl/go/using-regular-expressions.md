---
title:                "Używanie wyrażeń regularnych"
html_title:           "Go: Używanie wyrażeń regularnych"
simple_title:         "Używanie wyrażeń regularnych"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Dlaczego

Użycie wyrażeń regularnych jest nieodłączną częścią programowania w Go. Pozwala ono na precyzyjne wyszukiwanie i manipulację tekstem w sposób prostszy i szybszy niż napisanie własnej logiki. To niezastąpione narzędzie dla programisty, który chce osiągnąć dużą efektywność i elastyczność w swoim kodzie.

## Jak to zrobić

Użycie wyrażeń regularnych w Go jest proste i wymaga jedynie kilku kroków. Najpierw musimy zaimportować pakiet `regexp`, który zawiera funkcje i metody potrzebne do pracy z wyrażeniami regularnymi.

Następnie musimy skompilować nasze wyrażenie za pomocą funkcji `regexp.Compile()` i przypisać je do zmiennej. W przykładzie poniżej użyjemy wyrażenia regularnego `dawaj [a-z]+!`, które będzie odpowiadać wyrazom zaczynającym się od "dawaj" i kończących się znakiem "!".

```Go
package main

import "fmt"
import "regexp"

func main() {
    // kompilacja wyrażenia regularnego
    re := regexp.MustCompile(`dawaj [a-z]+!`)

    // przeszukiwanie tekstu
    fmt.Println(re.MatchString("dawaj piwo!")) // zwraca true
    fmt.Println(re.MatchString("dawaj ciastko!")) // zwraca true
    fmt.Println(re.MatchString("daj mi prezent!")) // zwraca false

    // pobieranie dopasowanych wyrażeń
    fmt.Println(re.FindString("dawaj bukiet!")) // zwraca "dawaj bukiet!"
    fmt.Println(re.FindString("proszę, dawaj mi przerwę!")) // zwraca "dawaj mi"
}
```

Jak widać, użycie wyrażeń regularnych pozwala nam precyzyjnie wyszukiwać i pobierać dopasowane wyrażenia z tekstu. Dzięki temu jest to bardzo przydatne narzędzie w wielu scenariuszach programistycznych.

## Głębszy zanurzenie

Podczas pracy z wyrażeniami regularnymi w Go warto zwrócić uwagę na kilka dodatkowych funkcji i metod. Przydatną funkcją jest `regexp.MustCompilePOSIX()`, która pozwala na użycie wyrażeń regularnych zgodnych z standardami POSIX. Jest to przydatne w przypadku korzystania z systemowych poleceń do przetwarzania tekstu.

Jeśli potrzebujemy odwołać się do konkretnego dopasowanego podwyrażenia, możemy użyć metody `re.FindSubmatch()`. Pozwala ona na pobranie dopasowanej frazy wraz z grupą wartości, które zostały ujęte w nawiasy w wyrażeniu regularnym. Jest to szczególnie przydatne w bardziej złożonych wyrażeniach.

Oprócz tego, pakiet `regexp` oferuje wiele innych metod do przetwarzania tekstów z wykorzystaniem wyrażeń regularnych, takich jak `FindAll()`, `ReplaceAll()` czy `Split()`. Warto przejrzeć dokumentację i poznać wszystkie dostępne możliwości.

## Zobacz także

Chcesz dowiedzieć się więcej o użyciu wyrażeń regularnych w Go? Sprawdź te przydatne materiały:

- Dokumentacja pakietu `regexp`: https://golang.org/pkg/regexp/
- Wyrażenia regularne w akcji: https://gobyexample.com/regular-expressions
- Przydatny poradnik o wyrażeniach regularnych w Go: https://www.digitalocean.com/community/tutorials/how-to-use-regular-expressions-in-go-pl