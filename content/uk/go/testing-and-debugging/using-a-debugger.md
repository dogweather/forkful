---
title:                "Використання відлагоджувача"
date:                  2024-02-03T18:10:40.935928-07:00
model:                 gpt-4-0125-preview
simple_title:         "Використання відлагоджувача"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/using-a-debugger.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та Чому?

Використання зневаджувача в програмуванні на Go передбачає застосування інструментів або функцій для перевірки та зміни стану запущеної програми з метою розуміння її поведінки або діагностики проблем. Програмісти роблять це для ефективного пошуку та усунення помилок, оптимізації продуктивності та забезпечення коректності свого коду.

## Як:

Go пропонує вбудований засіб для зневадження, що називається `delve`. Це повнофункціональний інструмент зневадження, який дозволяє вам виконувати програми на Go крок за кроком, перевіряти змінні програми та оцінювати вирази.

Для початку, спершу потрібно встановити `delve`. Це можна зробити, виконавши:

```shell
go get -u github.com/go-delve/delve/cmd/dlv
```

Тепер, давайте зневадимо просту програму на Go. Розглянемо програму `main.go`:

```go
package main

import "fmt"

func main() {
    message := "Debugging in Go"
    fmt.Println(message)
}
```

Щоб почати зневадження цієї програми, відкрийте термінал у директорії проекту та виконайте:

```shell
dlv debug
```

Ця команда компілює програму з вимкненими оптимізаціями (для покращення досвіду зневадження), запускає її та приєднує до неї зневаджувач.

Одразу після запуску `delve`, ви перебуваєте в інтерактивній оболонці зневаджувача. Ось кілька базових команд:

- `break main.main` встановлює точку зупинки на функції `main`.
- `continue` відновлює виконання програми до досягнення точки зупинки.
- `print message` виводить значення змінної `message`.
- `next` продовжує виконання програми до наступного рядка.
- `quit` виходить із зневаджувача.

Вивід даних при досягненні точки зупинки та виведенні змінної може виглядати так:

```shell
Breakpoint 1 at 0x49ecf3 for main.main() ./main.go:6
> main.main() ./main.go:6 (hits goroutine(1):1 total:1) (PC: 0x49ecf3)
     1: package main
     2:
     3: import "fmt"
     4:
     5: func main() {
     6: =>    message := "Debugging in Go"
     7:       fmt.Println(message)
     8: }
(dlv) print message
"Debugging in Go"
```

Використовуючи ці команди, ви можете просуватися крок за кроком через вашу програму, перевіряючи стан, щоб зрозуміти, як вона поводиться, та ідентифікувати будь-які проблеми.

## Поглиблений Розгляд

Вибір `delve` як інструменту зневадження для Go насамперед зумовлений особливостями моделі виконання та середовища виконання Go. GDB (GNU Debugger) спочатку не був розроблений з урахуванням середовища виконання Go, що робить `delve` більш підходящим вибором для розробників на Go. `Delve` спеціально розроблений для Go, пропонуючи більш інтуїтивно зрозумілий досвід зневадження для горутин, каналів та інших специфічних конструкцій Go.

Крім того, `delve` підтримує широкий спектр функцій, які виходять за рамки основних можливостей GDB при роботі з програмами на Go. Це включає, але не обмежується, приєднанням до запущених процесів для зневадження; умовними точками зупинки; та оцінкою складних виразів, які можуть включати примітиви конкуренції Go.

Хоча `delve` є основним зневаджувачем для багатьох розробників на Go, варто зауважити, що інструментарій Go також включає більш легкі форми підтримки зневадження, такі як вбудований інструмент `pprof` для профілювання та інструмент `trace` для візуалізації конкуренції. Ці інструменти іноді можуть забезпечити швидший або більш високорівневий шлях для діагностики проблем з продуктивністю програми або помилок конкуренції, що може бути доповненням або навіть кращим вибором залежно від контексту зневадження.