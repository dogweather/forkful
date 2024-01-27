---
title:                "Написання тестів"
date:                  2024-01-19
html_title:           "Arduino: Написання тестів"
simple_title:         "Написання тестів"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/writing-tests.md"
---

{{< edit_this_page >}}

## Що це таке та навіщо?

Тестування коду — це перевірка, чи правильно він працює. Робимо тести, щоб упевнитися, що наш програмний продукт без помилок і витримає майбутні зміни.

## Як це робити:

Go має вбудовану бібліотеку `testing`. Створимо тест для функції, що додає два числа:

```Go
package main

import (
    "testing"
    "fmt"
)

func Add(a int, b int) int {
    return a + b
}

func TestAdd(t *testing.T) {
    result := Add(2, 3)
    if result != 5 {
        t.Errorf("Expected 5, got %d", result)
    }
}
```

Для запуску тестів використовуй команду:

```
go test
```

Результат:

```
ok      example.com/mypackage   0.001s
```

## Поглиблено:

Тестування у Go з'явилося разом з мовою і наголошує на важливості самотестованих програм. Є альтернативи `testing`, наприклад, `Ginkgo` та `Testify`, які пропонують різні підходи та можливості. Тонкості написання тестів включають моки, бенчмарки та впровадження залежностей для ефективнішого тестування.

## Дивіться також:

- Офіційна документація по тестуванню в Go: https://golang.org/pkg/testing/
- Як писати бенчмарки в Go: https://golang.org/pkg/testing/#hdr-Benchmarks
- Бібліотека Testify для розширеного асертів і моків: https://github.com/stretchr/testify
- Інтро до тестування Go з Ginkgo: https://onsi.github.io/ginkgo/
