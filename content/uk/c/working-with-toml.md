---
title:                "Робота з TOML"
date:                  2024-01-26T04:20:10.579961-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/working-with-toml.md"
---

{{< edit_this_page >}}

## Що і чому?
TOML - це мова серіалізації даних, розроблена для того, щоб її було легко читати і писати. Програмісти використовують її для файлів конфігурації, простого зберігання даних та обміну даними між мовами завдяки її ясності та дружелюбності до людини.

## Як це зробити:
Давайте розберемо файл конфігурації TOML в C за допомогою бібліотеки "tomlc99". Спочатку встановіть бібліотеку. Тоді створіть `config.toml`:

```toml
title = "Приклад TOML"

[owner]
name = "Том Престон-Вернер"
dob = 1979-05-27T07:32:00Z
```

Тепер розберемо його в C:

```c
#include <stdio.h>
#include "toml.h"

int main() {
    FILE* fp;
    char errbuf[200];

    if (0 == (fp = fopen("config.toml", "r"))) {
        printf("Помилка: не вдається відкрити файл конфігурації\n");
        return 1;
    }
    
    toml_table_t* conf = toml_parse_file(fp, errbuf, sizeof(errbuf));
    fclose(fp);
    if (0 == conf) {
        printf("Помилка: %s\n", errbuf);
        return 1;
    }

    printf("Назва: %s\n", toml_raw_in(conf, "title"));

    toml_table_t* owner = toml_table_in(conf, "owner");
    printf("Ім'я власника: %s\n", toml_raw_in(owner, "name"));

    toml_free(conf);
    return 0;
}
```
Приклад виводу:
```
Назва: "Приклад TOML"
Ім'я власника: "Том Престон-Вернер"
```

## Поглиблений огляд
TOML, що означає Мова Тома, Ясна, Мінімальна, був створений Томом Престон-Вернером у 2013 році. Він слугує як простіша альтернатива форматам, таким як XML і YAML, зосереджуючись на більшій читабельності і можливості письма для людини. Хоча JSON є іншою альтернативою, TOML зберігає структуру, яка легше візуально розбирається людьми, що є однією з основних причин його використання у файлах конфігурації.

Працюючи з TOML в C, маємо обрати бібліотеку аналізатора, оскільки мова не підтримує його нативно. Бібліотеки, такі як "tomlc99", відповідають стандарту C99 і надають API для декодування текстів TOML. При розгляді продуктивності, належна обробка помилок та управління пам'яттю є критичними, оскільки C не має вбудованого збирання сміття.

## Див. також:
1. Специфікація TOML: [https://toml.io/en/](https://toml.io/en/)
2. GitHub репозиторій tomlc99: [https://github.com/cktan/tomlc99](https://github.com/cktan/tomlc99)
3. Порівняння форматів серіалізації даних: [https://labs.eleks.com/2015/07/comparison-of-data-serialization-formats.html](https://labs.eleks.com/2015/07/comparison-of-data-serialization-formats.html)
