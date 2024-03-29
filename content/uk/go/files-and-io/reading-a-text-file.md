---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:06:33.921232-07:00
description: "\u0427\u0438\u0442\u0430\u043D\u043D\u044F \u0442\u0435\u043A\u0441\u0442\
  \u043E\u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0443 \u0432 Go \u0432\u043A\
  \u043B\u044E\u0447\u0430\u0454 \u0434\u043E\u0441\u0442\u0443\u043F \u0442\u0430\
  \ \u043E\u0442\u0440\u0438\u043C\u0430\u043D\u043D\u044F \u0432\u043C\u0456\u0441\
  \u0442\u0443 \u0437 \u0444\u0430\u0439\u043B\u0443, \u0437\u0431\u0435\u0440\u0435\
  \u0436\u0435\u043D\u043E\u0433\u043E \u043D\u0430 \u0434\u0438\u0441\u043A\u0443\
  , \u0434\u043B\u044F \u043E\u0431\u0440\u043E\u0431\u043A\u0438 \u0430\u0431\u043E\
  \ \u0430\u043D\u0430\u043B\u0456\u0437\u0443. \u041F\u0440\u043E\u0433\u0440\u0430\
  \u043C\u0456\u0441\u0442\u0438 \u0447\u0430\u0441\u0442\u043E \u0432\u0438\u043A\
  \u043E\u043D\u0443\u044E\u0442\u044C \u0446\u044E\u2026"
lastmod: '2024-03-13T22:44:48.466142-06:00'
model: gpt-4-0125-preview
summary: "\u0427\u0438\u0442\u0430\u043D\u043D\u044F \u0442\u0435\u043A\u0441\u0442\
  \u043E\u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0443 \u0432 Go \u0432\u043A\
  \u043B\u044E\u0447\u0430\u0454 \u0434\u043E\u0441\u0442\u0443\u043F \u0442\u0430\
  \ \u043E\u0442\u0440\u0438\u043C\u0430\u043D\u043D\u044F \u0432\u043C\u0456\u0441\
  \u0442\u0443 \u0437 \u0444\u0430\u0439\u043B\u0443, \u0437\u0431\u0435\u0440\u0435\
  \u0436\u0435\u043D\u043E\u0433\u043E \u043D\u0430 \u0434\u0438\u0441\u043A\u0443\
  , \u0434\u043B\u044F \u043E\u0431\u0440\u043E\u0431\u043A\u0438 \u0430\u0431\u043E\
  \ \u0430\u043D\u0430\u043B\u0456\u0437\u0443. \u041F\u0440\u043E\u0433\u0440\u0430\
  \u043C\u0456\u0441\u0442\u0438 \u0447\u0430\u0441\u0442\u043E \u0432\u0438\u043A\
  \u043E\u043D\u0443\u044E\u0442\u044C \u0446\u044E\u2026"
title: "\u0427\u0438\u0442\u0430\u043D\u043D\u044F \u0442\u0435\u043A\u0441\u0442\u043E\
  \u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0443"
---

{{< edit_this_page >}}

## Що і чому?

Читання текстового файлу в Go включає доступ та отримання вмісту з файлу, збереженого на диску, для обробки або аналізу. Програмісти часто виконують цю операцію для маніпуляцій з даними, конфігурації програм або читання вводу для виконання програми, що робить це фундаментальною навичкою в розробці програмного забезпечення.

## Як:

Читання текстового файлу в Go можна виконати декількома способами, але один з найпростіших методів – використання пакету `ioutil`. Ось базовий приклад:

```go
package main

import (
    "fmt"
    "io/ioutil"
    "log"
)

func main() {
    content, err := ioutil.ReadFile("example.txt")
    if err != nil {
        log.Fatal(err)
    }

    fmt.Println(string(content))
}
```

Припускаючи, що `example.txt` містить "Hello, Go!", ця програма виведе:

```
Hello, Go!
```

Проте, починаючи з Go 1.16, пакет `ioutil` був застарілим, і рекомендується використовувати пакети `os` та `io` замість нього. Ось як ви можете досягти того ж з цими пакетами:

```go
package main

import (
    "bufio"
    "fmt"
    "log"
    "os"
)

func main() {
    file, err := os.Open("example.txt")
    if err != nil {
        log.Fatal(err)
    }
    defer file.Close()

    scanner := bufio.NewScanner(file)
    for scanner.Scan() {
        fmt.Println(scanner.Text())
    }

    if err := scanner.Err(); err != nil {
        log.Fatal(err)
    }
}
```

Цей підхід не тільки сучасніший, але й підтримує більші файли, оскільки читає файл рядок за рядком, замість завантаження всього вмісту у пам'ять одразу.

## Поглиблено:

Обробка операцій з файлами в Go, включаючи читання з файлів, відображає філософію мови щодо простоти та ефективності. Початково пакет `ioutil` пропонував прості операції з файлами. Проте, з покращеннями в стандартній бібліотеці Go та зміщенням акцентів на більш явне управління помилками та управління ресурсами, пакети `os` та `io` стали переважними альтернативами для роботи з файлами.

Ці зміни підкреслюють зобов'язання Go до продуктивності та безпеки, особливо уникнення проблем з пам'яттю, які можуть виникнути при завантаженні великих файлів цілком. Метод `bufio.Scanner`, запроваджений для читання файлів рядок за рядком, підкреслює адаптивність мови та зосередженість на сучасних викликах обчислювальних технологій, таких як обробка великих наборів даних або потокові дані.

Хоча існують зовнішні бібліотеки для роботи з файлами в Go, можливості стандартної бібліотеки часто достатні та віддають перевагу за їх стабільність та продуктивність. Це гарантує, що розробники Go можуть ефективно управляти операціями з файлами без необхідності покладатися на додаткові залежності, відповідаючи загальному мінімалістичному етосу та дизайну мови для створення ефективного, надійного програмного забезпечення.
