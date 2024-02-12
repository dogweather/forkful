---
title:                "Interpolacja ciągu znaków"
date:                  2024-02-03T17:58:40.659650-07:00
model:                 gpt-4-0125-preview
simple_title:         "Interpolacja ciągu znaków"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/interpolating-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

Interpolacja łańcuchów to metoda konstruowania łańcuchów, które włączają zmienne, umożliwiając dynamiczne tworzenie łańcuchów. Programiści robią to, aby dostosować komunikaty, konstruować adresy URL, tworzyć zapytania SQL i więcej, co pozwala na bardziej czytelny i łatwiejszy do utrzymania kod.

## Jak to zrobić:

W Go, interpolacja łańcuchów jest powszechnie osiągana za pomocą pakietu `fmt`, w szczególności funkcji `Sprintf`, która pozwala wstrzyknąć zmienne do łańcucha poprzez określenie słów formatujących. Słowa te są symbolami zastępczymi w ciągu formatu i są zastępowane przez wartości danych zmiennych. Oto jak tego użyć:

```go
package main

import (
    "fmt"
)

func main() {
    name := "Jane"
    age := 28

    // Używanie Sprintf do interpolacji łańcucha
    message := fmt.Sprintf("Cześć, mam na imię %s i mam %d lat.", name, age)
    fmt.Println(message) // Wyjście: Cześć, mam na imię Jane i mam 28 lat.
}
```

Zauważ, że `%s` jest używane dla łańcuchów, a `%d` dla liczb całkowitych. Dokumentacja pakietu `fmt` zawiera kompleksową listę słów formatujących dla różnych typów danych.

## Wgłębienie się

Koncepcja interpolacji łańcuchów istnieje w wielu językach programowania, choć z różnymi składniami i możliwościami. W Go, chociaż funkcja `Sprintf` z pakietu `fmt` jest najczęściej używanym podejściem, może nie zawsze być najbardziej wydajna, szczególnie w przypadku prostych konkatenacji lub gdy pracuje się w kodzie o wysokiej wrażliwości na wydajność.

Pakiet `fmt` używa refleksji do dynamicznej interpretacji typów zmiennych w czasie wykonania, co, chociaż elastyczne, wiąże się ze zwiększonym narzutem. W scenariuszach, gdzie wydajność jest krytyczna, bezpośrednia konkatenacja łańcuchów lub typ `strings.Builder` mogą oferować lepsze alternatywy. Bezpośrednia konkatenacja jest prosta, ale może stać się nieporęczna z wieloma zmiennymi. `strings.Builder`, z drugiej strony, zapewnia bardziej wydajny i czytelny sposób na budowanie złożonych łańcuchów w pętli lub przy obchodzeniu się z wieloma zmiennymi:

```go
var sb strings.Builder
sb.WriteString("Cześć, mam na imię ")
sb.WriteString(name)
sb.WriteString(" i mam ")
sb.WriteString(strconv.Itoa(age))
sb.WriteString(" lat.")
message := sb.String()

fmt.Println(message) // Wyświetla to samo co wcześniej
```

Ostatecznie, wybór między `fmt.Sprintf`, bezpośrednią konkatenacją a `strings.Builder` zależy od specyficznych wymagań Twojej aplikacji, takich jak złożoność konstruowanego łańcucha i rozważania dotyczące wydajności.