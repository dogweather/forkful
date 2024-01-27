---
title:                "Обробка помилок"
date:                  2024-01-26T00:53:05.290666-07:00
model:                 gpt-4-1106-preview
simple_title:         "Обробка помилок"
programming_language: "Go"
category:             "Go"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/handling-errors.md"
---

{{< edit_this_page >}}

## Що і Чому?

Обробка помилок в Go полягає у граційному вловлюванні та реагуванні на несподіванки під час виконання. Ми робимо це, щоб запобігти збоям і забезпечити передбачувану поведінку наших програм, навіть коли щось іде не так.

## Як це зробити:

Go використовує явне керування помилками. Це означає, що ви перевіряєте наявність помилки після кожного виклику функції. Без винятків. Ось як це виглядає:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	err := doSomething()
	if err != nil {
		fmt.Println("Ой-ой:", err)
		os.Exit(1)
	}
}

func doSomething() error {
	// Прикидаємо, що сталася помилка
	return fmt.Errorf("щось пішло не так")
}
```

Якщо це запустити, то отримаємо:

```
Ой-ой: щось пішло не так
```

Але що, якщо воно виконається успішно?

```Go
func doSomething() error {
	// Цього разу все гаразд
	return nil
}
```

Ніякого виводу. Круто, жодних новин - це добра новина.

## Поглиблений Аналіз:

В Go обробка помилок завжди була предметом дискусій. З самого початку Go зробив ставку проти винятків на користь більш явного підходу, який деякі розробники поцінували за простоту, а інші вважають важким. Вбудований тип `error` є інтерфейсом. Будь-який тип з методом `Error() string` виконує його. Це відповідає етосу Go - простоти та явності.

Альтернативи? Є парочка `panic` і `recover`, але вони призначені для виняткових випадків (гра слів задумана), коли програма не може продовжувати роботу. Думайте про `panic` як про кнопку викидання, яку натискаєте, коли розумієте, що немає вороття назад. Використовуйте це рідко.

Що стосується основної обробки помилок, Go 1.13 ввів обгортання помилок, що спрощує розуміння "ланцюжка помилок" завдяки функціям типу `errors.Is()` та `errors.As()`.

## Дивіться Також:

Для всіх аспектів обробки помилок в Go:

- Блог Go про обробку помилок: [https://blog.golang.org/error-handling-and-go](https://blog.golang.org/error-handling-and-go)
- Effective Go – розділ про обробку помилок: [https://golang.org/doc/effective_go#errors](https://golang.org/doc/effective_go#errors)
- Документація обгортання помилок Go 1.13: [https://golang.org/doc/go1.13#error_wrapping](https://golang.org/doc/go1.13#error_wrapping)
- Пост Дейва Чені про стратегії обробки помилок: [https://dave.cheney.net/2016/04/27/dont-just-check-errors-handle-them-gracefully](https://dave.cheney.net/2016/04/27/dont-just-check-errors-handle-them-gracefully)