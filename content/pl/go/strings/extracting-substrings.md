---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:56:47.144020-07:00
description: "Jak to zrobi\u0107: W Go typ `string` jest tylko do odczytu wycinkiem\
  \ (slice) bajt\xF3w. Aby wyodr\u0119bni\u0107 podci\u0105gi, g\u0142\xF3wnie u\u017C\
  ywa si\u0119 sk\u0142adni `slice` wraz z\u2026"
lastmod: '2024-03-13T22:44:34.837004-06:00'
model: gpt-4-0125-preview
summary: "W Go typ `string` jest tylko do odczytu wycinkiem (slice) bajt\xF3w."
title: "Wydobywanie podci\u0105g\xF3w"
weight: 6
---

## Jak to zrobić:
W Go typ `string` jest tylko do odczytu wycinkiem (slice) bajtów. Aby wyodrębnić podciągi, głównie używa się składni `slice` wraz z wbudowaną funkcją `len()` do sprawdzania długości oraz pakietu `strings` dla bardziej złożonych operacji. Oto jak możesz to osiągnąć:

### Podstawowe wycinanie
```go
package main

import (
    "fmt"
)

func main() {
    str := "Hello, World!"
    // Wydobywa "World"
    subStr := str[7:12]
    
    fmt.Println(subStr) // Wynik: World
}
```

### Użycie pakietu `strings`
Dla bardziej zaawansowanego wyodrębniania podciągów, takiego jak ekstrakcja ciągów po lub przed określonym podciągiem, możesz użyć pakietu `strings`.

```go
package main

import (
    "fmt"
    "strings"
)

func main() {
    str := "name=John Doe"
    // Ekstrakcja podciągu po "="
    subStr := strings.SplitN(str, "=", 2)[1]
    
    fmt.Println(subStr) // Wynik: John Doe
}
```

Należy zauważyć, że ciągi Go są kodowane w UTF-8 i bezpośredni wycinek bajtów nie zawsze może dać prawidłowe ciągi, jeśli zawierają znaki wielobajtowe. Dla wsparcia Unicode, rozważ użycie `range` lub pakietu `utf8`.

### Obsługa znaków Unicode
```go
package main

import (
    "fmt"
    "unicode/utf8"
)

func main() {
    str := "Hello, 世界"
    // Znalezienie podciągu uwzględniając znaki Unicode
    runeStr := []rune(str)
    subStr := string(runeStr[7:])
    
    fmt.Println(subStr) // Wynik: 世界
}
```

## Dogłębna analiza
Wyodrębnianie podciągów w Go jest proste dzięki jego składni wycinania i obszernej bibliotece standardowej. Historycznie, wcześniejsze języki programowania oferowały bardziej bezpośrednie funkcje lub metody do obsługi takich manipulacji tekstem. Jednak podejście Go kładzie nacisk na bezpieczeństwo i wydajność, szczególnie ze względu na jego niezmienne ciągi i jawne obsługiwanie znaków Unicode przez runy.

Chociaż proste wycinanie ma zalety w postaci wydajności, dziedziczy ono złożoności związane z bezpośrednim obsługiwaniem znaków UTF-8. Wprowadzenie typu `rune` pozwala programom w Go na bezpieczne obsługiwanie tekstu Unicode, czyniąc go potężną alternatywą dla aplikacji międzynarodowych.

Ponadto, programiści pochodzący z innych języków mogą tęsknić za wbudowanymi funkcjami wysokopoziomowej manipulacji ciągami. Jednak pakiety `strings` i `bytes` w standardowej bibliotece Go oferują bogaty zestaw funkcji, które, choć wymagają nieco więcej kodu, zapewniają potężne opcje przetwarzania ciągów, w tym wyodrębnianie podciągów.

W istocie, decyzje projektowe Go wokół manipulacji ciągami odzwierciedlają jego cele dotyczące prostoty, wydajności i bezpieczeństwa w radzeniu sobie z nowoczesnymi, zinternacjonalizowanymi danymi tekstowymi. Chociaż może to wymagać niewielkiej adaptacji, Go oferuje skuteczne i wydajne narzędzia do obsługi wyodrębniania podciągów i nie tylko.
