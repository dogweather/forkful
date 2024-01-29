---
title:                "Написание тестов"
date:                  2024-01-29T00:05:52.419024-07:00
model:                 gpt-4-0125-preview
simple_title:         "Написание тестов"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/go/writing-tests.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Написание тестов подразумевает создание кода для проверки работы другого кода. Программисты делают это, чтобы заранее обнаружить ошибки, обеспечить функциональность и избежать будущих проблем.

## Как:

В Go есть встроенный пакет для тестирования под названием `testing`. Продемонстрируем на примере функции `Add`, которая складывает два целых числа:

```Go
// add.go
package math

func Add(x, y int) int {
    return x + y
}
```

Напишите тест таким образом:

```Go
// add_test.go
package math

import (
    "testing"
)

func TestAdd(t *testing.T) {
    result := Add(1, 2)
    expected := 3
    if result != expected {
        t.Errorf("Add(1, 2) = %d; хотели %d", result, expected)
    }
}
```

Запустите тесты с помощью `go test`. Вы увидите вывод вроде:

```
PASS
ok      example.com/your-module/math   0.002s
```

## Подробнее

Go внедрил встроенное тестирование в 2011 году. Это проще, чем использование отдельной библиотеки. Вы пишете тесты в файлах `_test.go`, используя `testing.T` для сообщения о неудачах.

Альтернативы? Конечно, вы можете использовать Testify для утверждений, Ginkgo для BDD или GoCheck для более продвинутых функций. Но пакет `testing` не имеет зависимостей, прост в использовании и часто этого достаточно.

Под капотом `go test` компилирует ваш код и тесты вместе, запускает их и сообщает результаты. Это идиоматический Go: обычные случаи легки, особые случаи возможны.

## Смотрите также

Для дополнительной информации смотрите документацию:

- Пакет для тестирования: [https://pkg.go.dev/testing](https://pkg.go.dev/testing)
- Тесты на основе таблиц: [https://github.com/golang/go/wiki/TableDrivenTests](https://github.com/golang/go/wiki/TableDrivenTests)
