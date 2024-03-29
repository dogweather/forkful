---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:16:00.374581-07:00
description: "\u041D\u0430\u043F\u0438\u0441\u0430\u043D\u043D\u044F \u0442\u0435\u0441\
  \u0442\u0456\u0432 \u0443 Go \u0432\u043A\u043B\u044E\u0447\u0430\u0454 \u0441\u0442\
  \u0432\u043E\u0440\u0435\u043D\u043D\u044F \u043C\u0430\u043B\u0438\u0445, \u043A\
  \u0435\u0440\u043E\u0432\u0430\u043D\u0438\u0445 \u0448\u043C\u0430\u0442\u043A\u0456\
  \u0432 \u043A\u043E\u0434\u0443, \u044F\u043A\u0456 \u043F\u0435\u0440\u0435\u0432\
  \u0456\u0440\u044F\u044E\u0442\u044C \u0444\u0443\u043D\u043A\u0446\u0456\u043E\u043D\
  \u0430\u043B\u044C\u043D\u0456\u0441\u0442\u044C \u0442\u0430 \u043F\u043E\u0432\
  \u0435\u0434\u0456\u043D\u043A\u0443 \u0432\u0430\u0448\u043E\u0433\u043E \u0434\
  \u043E\u0434\u0430\u0442\u043A\u0443. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\
  \u0456\u0441\u0442\u0438 \u043F\u0438\u0448\u0443\u0442\u044C\u2026"
lastmod: '2024-03-13T22:44:48.441040-06:00'
model: gpt-4-0125-preview
summary: "\u041D\u0430\u043F\u0438\u0441\u0430\u043D\u043D\u044F \u0442\u0435\u0441\
  \u0442\u0456\u0432 \u0443 Go \u0432\u043A\u043B\u044E\u0447\u0430\u0454 \u0441\u0442\
  \u0432\u043E\u0440\u0435\u043D\u043D\u044F \u043C\u0430\u043B\u0438\u0445, \u043A\
  \u0435\u0440\u043E\u0432\u0430\u043D\u0438\u0445 \u0448\u043C\u0430\u0442\u043A\u0456\
  \u0432 \u043A\u043E\u0434\u0443, \u044F\u043A\u0456 \u043F\u0435\u0440\u0435\u0432\
  \u0456\u0440\u044F\u044E\u0442\u044C \u0444\u0443\u043D\u043A\u0446\u0456\u043E\u043D\
  \u0430\u043B\u044C\u043D\u0456\u0441\u0442\u044C \u0442\u0430 \u043F\u043E\u0432\
  \u0435\u0434\u0456\u043D\u043A\u0443 \u0432\u0430\u0448\u043E\u0433\u043E \u0434\
  \u043E\u0434\u0430\u0442\u043A\u0443. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\
  \u0456\u0441\u0442\u0438 \u043F\u0438\u0448\u0443\u0442\u044C\u2026"
title: "\u041D\u0430\u043F\u0438\u0441\u0430\u043D\u043D\u044F \u0442\u0435\u0441\u0442\
  \u0456\u0432"
---

{{< edit_this_page >}}

## Що і чому?

Написання тестів у Go включає створення малих, керованих шматків коду, які перевіряють функціональність та поведінку вашого додатку. Програмісти пишуть тести, щоб переконатися, що їхній код працює як очікується за різних умов, спростити рефакторинг і допомогти запобігти регресіям.

## Як:

У Go тести зазвичай пишуться в тому ж пакеті, що й код, який вони тестують. Файли, що містять тести, називаються з суфіксом `_test.go`. Тести - це функції, які приймають вказівник на об'єкт testing.T (з пакету `testing`) як аргумент, і вони сигналізують про невдачу, викликаючи методи, такі як `t.Fail()`, `t.Errorf()` тощо.

Приклад простого тесту для функції `Add`, визначеної у `math.go`:
```go
// math.go
package math

func Add(x, y int) int {
    return x + y
}
```

Тестовий файл `math_test.go`:
```go
package math

import "testing"

func TestAdd(t *testing.T) {
    result := Add(1, 2)
    expected := 3
    if result != expected {
        t.Errorf("Add(1, 2) = %d; хочеться %d", result, expected)
    }
}
```

Запустіть свої тести за допомогою команди `go test` у тій самій директорії, що й ваші тестові файли. Приклад виводу, що вказує на успішний тест, виглядатиме приблизно так:

```
PASS
ok      example.com/my/math 0.002s
```

Для тестів, що керуються таблицею, дозволяючи ефективно тестувати різні комбінації вхідних і вихідних даних, визначте зріз структур, що представляють випадки тестування:

```go
func TestAddTableDriven(t *testing.T) {
    var tests = []struct {
        x        int
        y        int
        expected int
    }{
        {1, 2, 3},
        {2, 3, 5},
        {-1, -2, -3},
    }

    for _, tt := range tests {
        testname := fmt.Sprintf("%d+%d", tt.x, tt.y)
        t.Run(testname, func(t *testing.T) {
            ans := Add(tt.x, tt.y)
            if ans != tt.expected {
                t.Errorf("отримано %d, хочеться %d", ans, tt.expected)
            }
        })
    }
}
```

## Поглиблений огляд

Фреймворк тестування Go, представлений в Go 1 разом з мовою, був розроблений для безперебійної інтеграції з інструментарієм Go, відображаючи наголос Go на простоті та ефективності в розробці програмного забезпечення. На відміну від деяких фреймворків тестування в інших мовах, які покладаються на зовнішні бібліотеки чи складні налаштування, вбудований пакет `testing` Go надає простий спосіб написання та запуску тестів.

Цікавим аспектом підходу Go до тестування є принцип "конвенція над конфігурацією", якого він дотримується, як-от шаблон назви файлів (`_test.go`) та використання стандартних можливостей бібліотеки замість зовнішніх залежностей. Цей мінімалістичний підхід заохочує розробників писати тести, оскільки вхідний бар'єр є низьким.

Хоча вбудовані можливості тестування Go охоплюють багато аспектів, існують сценарії, в яких сторонні інструменти або фреймворки можуть пропонувати більше функціональностей, такі як генерація мок-об'єктів, фазз-тестування або тести у стилі BDD (розробка на основі поведінки). Популярні бібліотеки, такі як Testify або GoMock, доповнюють стандартні можливості тестування Go, пропонуючи більш виразні перевірки чи можливості генерації мок-об'єктів, які можуть бути особливо корисними в складних програмах із багатьма залежностями.

Незважаючи на існування цих альтернатив, стандартний пакет тестування Go залишається кутовим каменем для тестування у Go завдяки його простоті, ефективності і тісній інтеграції з мовою і інструментарієм. Чи вирішують розробники доповнювати його сторонніми інструментами чи ні, фреймворк тестування Go надає міцну основу для забезпечення якості коду і надійності.
