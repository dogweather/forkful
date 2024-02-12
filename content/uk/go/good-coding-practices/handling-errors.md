---
title:                "Обробка помилок"
date:                  2024-02-03T17:58:44.039944-07:00
model:                 gpt-4-0125-preview
simple_title:         "Обробка помилок"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/handling-errors.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та чому?

Обробка помилок в Go полягає в розпізнаванні та реагуванні на умови помилок у вашій програмі. Програмісти займаються обробкою помилок, щоб забезпечити благополучне відновлення їх додатків у несподіваних ситуаціях, що призводить до більш надійного та стійкого програмного забезпечення.

## Як:

В Go обробка помилок явно керується за допомогою типу `error`. Функції, які можуть зазнати невдачі, повертають помилку як своє останнє значення повернення. Перевірка, чи це значення помилки є `nil`, скаже вам, чи сталася помилка.

```go
package main

import (
    "errors"
    "fmt"
)

func Compute(value int) (int, error) {
    if value > 100 {
        return 0, errors.New("значення має бути 100 або менше")
    }
    return value * 2, nil
}

func main() {
    result, err := Compute(150)
    if err != nil {
        fmt.Println("Помилка:", err)
    } else {
        fmt.Println("Результат:", result)
    }
    
    // Граційна обробка помилки
    anotherResult, anotherErr := Compute(50)
    if anotherErr != nil {
        fmt.Println("Помилка:", anotherErr)
    } else {
        fmt.Println("Результат:", anotherResult)
    }
}
```

Приклад виводу для наведеного вище коду:
```
Помилка: значення має бути 100 або менше
Результат: 100
```

У цьому прикладі, функція `Compute` або повертає обчислене значення, або помилку. Викликуюча сторона обробляє помилку, перевіряючи, що `err` не є `nil`.

## Поглиблений аналіз

Підхід Go до обробки помилок навмисно простий і типобезпечний, вимагаючи явних перевірок на помилки. Цей концепт контрастує з обробкою помилок на основі виключень, яка спостерігається в мовах на кшталт Java і Python, де помилки розповсюджуються вгору стеку викликів, доки їх не перехопить обробник виключень. Команда Go аргументує, що явне оброблення помилок призводить до чіткішого та більш надійного коду, оскільки це змушує програмістів негайно адресувати помилки там, де вони виникають.

Однак, деякі критики зазначають, що цей шаблон може призвести до надмірно деталізованого коду, особливо у складних функціях з багатьма операціями, схильними до помилок. У відповідь на це, новіші версії Go ввели більш витончені функції обробки помилок, такі як обгортання помилок, що робить легшим надання контексту помилці без втрати оригінальної інформації про помилку. Спільнота також бачила пропозиції щодо нових механізмів обробки помилок, таких як перевірка/обробка, хоча ці обговорення залишаються актуальними станом на мій останній оновлення.

Філософія обробки помилок в Go підкреслює розуміння та планування помилок як частини нормального потоку програми. Цей підхід спонукає до розробки більш стійкого та передбачуваного програмного забезпечення, хоча й потенційно збільшує кількість шаблонного коду. Існують альтернативні шаблони та бібліотеки для спрощення обробки помилок у особливо складних випадках, але вбудований тип `error` залишається основою обробки помилок у мові.