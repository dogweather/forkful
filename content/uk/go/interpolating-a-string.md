---
title:                "Інтерполяція рядка"
html_title:           "Java: Інтерполяція рядка"
simple_title:         "Інтерполяція рядка"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Що і Навіщо?

Інтерполяція рядків - це вбудовування змінних або виразів безпосередньо в рядок. Програмісти це роблять, щоб краще форматувати вивід або злити кілька рядкових виразів.

## Як це робити:

Спосіб інтерполяції рядків в Go через функцію `fmt.Sprintf()`. Скажімо, у вас є дві змінні `name := "Oleh"` та `role := "programmer"`. 

```Go
package main

import "fmt"

func main() {
        name := "Oleh"
        role := "programmer"
        sentence := fmt.Sprintf("%s is a %s.", name, role)
        fmt.Println(sentence)
}
```

Вивід:

```bash
Oleh is a programmer.
```

## Пірнемо глибше:

1. Історичний контекст: інтерполяція рядків була популярна у ряді мов програмування, зокрема, в Perl та Ruby, до появи Go.
2. Альтернативи: крім `fmt.Sprintf()`, можна використати `+` або `fmt.Printf()`, але ці методи менш гнучкі.
3. Деталі реалізації: `fmt.Sprintf()` не просто з'єднує рядки, вона використовує форматування під капотом, дозволяючи вставляти різноманітні типи даних.

## Див. також:

* [Детальніше про функцію Sprintf в Go] (https://golang.org/pkg/fmt/#Sprintf)
* [Iнтерполяція рядків в Perl](https://perlmaven.com/string-interpolation)
* [Iнтерполяція рядків в Ruby](https://www.rubyguides.com/2019/07/ruby-string-interpolation/)