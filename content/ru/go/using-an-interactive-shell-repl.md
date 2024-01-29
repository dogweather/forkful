---
title:                "Использование интерактивной оболочки (REPL)"
date:                  2024-01-29T00:03:30.698128-07:00
model:                 gpt-4-0125-preview
simple_title:         "Использование интерактивной оболочки (REPL)"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/go/using-an-interactive-shell-repl.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и зачем?
REPL (цикл "чтение-выполнение-вывод") позволяет взаимодействовать с кодом в реальном времени: он читает ввод, выполняет его, выводит результат и зацикливается. Программисты используют его для тестирования фрагментов кода, отладки и изучения новых языков в реальном времени.

## Как использовать:
Go не включает в себя встроенный REPL, но вы можете использовать сторонние инструменты. Один из популярных инструментов - `gore`:

```go
// Установка gore
$ go install github.com/motemen/gore/cmd/gore@latest

// Запуск gore
$ gore
gore версия 0.5.0  :help для помощи
gore> :import fmt
gore> fmt.Println("Привет, Go REPL!")
Привет, Go REPL!
nil
```

## Подробнее
Изначально разработанный для Lisp, REPLы распространены в динамических языках, таких как Python или Ruby. Go, будучи статически типизированным, из коробки не включает его. Альтернативы `gore` включают в себя `go-pry` и `yaegi`. Эти инструменты интерпретируют код Go, позволяя быстро исследовать и проверять идеи без компиляции полноценного приложения. Они особенно полезны для начинающих и в образовательных контекстах, где акцент сделан на обучении и экспериментировании.

## Смотрите также
- `gore`: [https://github.com/motemen/gore](https://github.com/motemen/gore)
- `go-pry`: [https://github.com/d4l3k/go-pry](https://github.com/d4l3k/go-pry)
- `yaegi`: [https://github.com/traefik/yaegi](https://github.com/traefik/yaegi)
