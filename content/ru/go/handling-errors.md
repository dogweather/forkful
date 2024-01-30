---
title:                "Обработка ошибок"
date:                  2024-01-28T23:58:47.685933-07:00
model:                 gpt-4-0125-preview
simple_title:         "Обработка ошибок"
programming_language: "Go"
category:             "Go"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/go/handling-errors.md"
changelog:
  - 2024-01-21, dogweather, Reviewed for accuracy
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?

Обработка ошибок в Go заключается в изящном перехвате и реагировании на проблемы во время выполнения. Мы делаем это, чтобы предотвратить сбои и обеспечить предсказуемую работу наших программ, даже когда происходят непредвиденные ситуации.

## Как это делается:

В Go используется явная обработка ошибок. Это означает, что вы будете проверять, возвращает ли функция ошибку каждый раз, когда вы ее вызываете. Без исключений. Вот как это выглядит:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	err := doSomething()
	if err != nil {
		fmt.Println("Упс:", err)
		os.Exit(1)
	}
}

func doSomething() error {
	// Представим, что что-то пошло не так
	return fmt.Errorf("что-то пошло не так")
}
```

Запустите это, и вы получите:

```
Упс: что-то пошло не так
```

А что если все прошло успешно?

```Go
func doSomething() error {
	// В этот раз все хорошо
	return nil
}
```

Нет вывода. Классно, нет новостей - это хорошие новости.

## Глубокое погружение:

В Go обработка ошибок всегда была предметом споров. С самого начала Go решил отказаться от исключений в пользу более явного подхода, который некоторые разработчики любят за его простоту, а другие считают многословным. Встроенный тип `error` является интерфейсом. Любой тип с методом `Error() string` удовлетворяет ему. Это сочетается с духом Go в плане простоты и явности.

Альтернативы? Есть пара `panic` и `recover`, но они предназначены для исключительных случаев (игра слов), когда программа не может продолжать работу. Подумайте о `panic` как о кнопке выброса, которую вы нажимаете, когда знаете, что обратного пути нет. Используйте это с умом.

Что касается основной обработки ошибок, то в Go 1.13 была введена обработка ошибок через обертывание, что упростило определение "цепочки ошибок" с функциями вроде `errors.Is()` и `errors.As()`.

## Смотрите также:

Для всего, что касается обработки ошибок в Go:

- Блог Go об обработке ошибок: [https://blog.golang.org/error-handling-and-go](https://blog.golang.org/error-handling-and-go)
- Эффективный Go – раздел обработки ошибок: [https://golang.org/doc/effective_go#errors](https://golang.org/doc/effective_go#errors)
- Документация по обертыванию ошибок в Go 1.13: [https://golang.org/doc/go1.13#error_wrapping](https://golang.org/doc/go1.13#error_wrapping)
- Пост Дэйва Чейни об стратегиях обработки ошибок: [https://dave.cheney.net/2016/04/27/dont-just-check-errors-handle-them-gracefully](https://dave.cheney.net/2016/04/27/dont-just-check-errors-handle-them-gracefully)