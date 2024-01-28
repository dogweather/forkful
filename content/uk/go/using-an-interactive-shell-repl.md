---
title:                "Використання інтерактивної оболонки (REPL)"
date:                  2024-01-26T04:15:01.580366-07:00
model:                 gpt-4-0125-preview
simple_title:         "Використання інтерактивної оболонки (REPL)"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Що та Чому?
REPL (цикл "читати-оцінити-вивести-повторити") дозволяє взаємодіяти з кодом наживо; він читає введення, оцінює його, виводить результат і повертається назад. Програмісти використовують його для тестування фрагментів, виправлення помилок та вивчення нових мов у реальному часі.

## Як користуватися:
Go не має вбудованого REPL, але ви можете використовувати інструменти сторонніх розробників. Один із популярних інструментів - `gore`:

```go
// Встановити gore за допомогою
$ go install github.com/motemen/gore/cmd/gore@latest

// Запустити gore
$ gore
gore version 0.5.0  :help для допомоги
gore> :import fmt
gore> fmt.Println("Hello, Go REPL!")
Привіт, Go REPL!
nil
```

## Поглиблений огляд
Вперше розроблений для Lisp, REPLи поширені в динамічних мовах, як-от Python чи Ruby. Go, будучи статично типізованою мовою, не має такого інструменту "з коробки". Альтернативи `gore` включають `go-pry` і `yaegi`. Ці інструменти інтерпретують код Go, дозволяючи швидко досліджувати та перевіряти ідеї без компіляції повноцінного додатку. Вони особливо корисні для початківців і в освітніх контекстах, де акцент робиться на вивченні та експериментах.

## Дивіться також
- `gore`: [https://github.com/motemen/gore](https://github.com/motemen/gore)
- `go-pry`: [https://github.com/d4l3k/go-pry](https://github.com/d4l3k/go-pry) 
- `yaegi`: [https://github.com/traefik/yaegi](https://github.com/traefik/yaegi)
