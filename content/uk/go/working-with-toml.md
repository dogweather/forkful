---
title:                "Робота з TOML"
date:                  2024-01-26T04:22:57.622373-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з TOML"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/working-with-toml.md"
---

{{< edit_this_page >}}

## Що і Чому?
Робота з TOML включає парсинг та кодування файлів TOML (Tom's Obvious, Minimal Language) у Go. Програмісти обирають TOML за його читабельність та легкість відображення до структур даних, що ідеально підходить для конфігурацій.

## Як:
Для роботи з TOML у Go ви зазвичай будете використовувати бібліотеку, як от `BurntSushi/toml`. Ось швидкий погляд на парсинг конфігураційного файлу TOML:

```Go
package main

import (
    "fmt"
    "os"

    "github.com/BurntSushi/toml"
)

type Config struct {
    Title   string
    Owner   struct {
        Name string
    }
}

func main() {
    var config Config
    if _, err := toml.DecodeFile("config.toml", &config); err != nil {
        fmt.Println(err)
        return
    }
    fmt.Printf("Title: %s, Owner: %s\n", config.Title, config.Owner.Name)
}
```

Зразок `config.toml`:

```Toml
title = "Example TOML"
[owner]
name = "Tom Preston-Werner"
```

Зразок виводу:

```
Title: Example TOML, Owner: Tom Preston-Werner
```

## Поглиблене Вивчення
TOML, представлений Томом Престон-Вернером у 2013 році, був розроблений як мінімалістичний формат файлу конфігурації, що легко читається завдяки своїй зрозумілій семантиці. Розробники Go часто використовують TOML для конфігурації, віддаючи перевагу йому перед альтернативами, як-от JSON або YAML, за його простоту та здатність представляти складні ієрархії з легкістю.

На відміну від YAML, який має складні функції та потенційні питання безпеки, плоский дизайн TOML зменшує складність та помилки через друк. І на відміну від JSON, TOML підтримує коментарі, що робить його легшим для пояснення конфігурацій в рядках.

При роботі з TOML у Go є нюанси, про які варто знати. Теги структур дозволяють налаштовувати, як ваші структури відображаються на структури TOML, і ви також повинні бути обізнані про те, як масиви та вбудовані таблиці TOML аналізуються в зрізи та мапи Go.

## Див. також
- Специфікація TOML: https://toml.io/en/
- Бібліотека BurntSushi/toml: https://github.com/BurntSushi/toml
- Порівняння форматів файлів конфігурації: https://www.redhat.com/sysadmin/yaml-toml-json-differences
