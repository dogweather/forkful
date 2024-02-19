---
aliases:
- /pl/go/interpolating-a-string/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:58:40.659650-07:00
description: "Interpolacja \u0142a\u0144cuch\xF3w to metoda konstruowania \u0142a\u0144\
  cuch\xF3w, kt\xF3re w\u0142\u0105czaj\u0105 zmienne, umo\u017Cliwiaj\u0105c dynamiczne\
  \ tworzenie \u0142a\u0144cuch\xF3w. Programi\u015Bci robi\u0105 to, aby\u2026"
lastmod: 2024-02-18 23:08:49.080919
model: gpt-4-0125-preview
summary: "Interpolacja \u0142a\u0144cuch\xF3w to metoda konstruowania \u0142a\u0144\
  cuch\xF3w, kt\xF3re w\u0142\u0105czaj\u0105 zmienne, umo\u017Cliwiaj\u0105c dynamiczne\
  \ tworzenie \u0142a\u0144cuch\xF3w. Programi\u015Bci robi\u0105 to, aby\u2026"
title: "Interpolacja ci\u0105gu znak\xF3w"
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
