---
title:                "Робота з TOML"
aliases:
- /uk/c/working-with-toml.md
date:                  2024-02-03T18:12:50.619452-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з TOML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/working-with-toml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та чому?

TOML (Tom's Obvious, Minimal Language - Очевидна, Мінімалістична Мова Тома) - це формат файлу конфігурації, який легко читати завдяки своїй чіткій семантиці. Програмісти використовують його для конфігураційних файлів у програмах, оскільки його простота та зручність для читання роблять його відмінним вибором над форматами, такими як XML або JSON, у певних контекстах.

## Як:

Щоб працювати з TOML у C, спочатку вам потрібна бібліотека, здатна парсити файли TOML, оскільки стандартна бібліотека C не включає цю функціональність. Популярним вибором є `tomlc99`, легковаговий парсер TOML для C99. Ось швидкий гід по читанню простого конфігураційного файлу TOML:

Спочатку переконайтеся, що у вас встановлений і правильно підключений у вашому проєкті `tomlc99`.

**Зразок файлу TOML (`config.toml`):**
```toml
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
```

**Код на С для парсингу цього файлу:**

```c
#include <stdio.h>
#include <stdlib.h>
#include "toml.h"

int main() {
    FILE *configFile;
    configFile = fopen("config.toml", "r");
    if (!configFile) {
        perror("Cannot open file");
        return EXIT_FAILURE;
    }

    toml_table_t *config = toml_parse_file(configFile, NULL, 0);
    if (!config) {
        fprintf(stderr, "Error parsing file\n");
        fclose(configFile);
        return EXIT_FAILURE;
    }

    toml_table_t *database = toml_table_in(config, "database");
    if (database) {
        const char *server = toml_raw_in(database, "server");
        printf("Database Server: %s\n", server);

        toml_array_t *ports = toml_array_in(database, "ports");
        for (int i = 0; i < toml_array_nelem(ports); i++) {
            int64_t port;
            toml_int_at(ports, i, &port);
            printf("Port %d: %ld\n", i, port);
        }
    }

    toml_free(config);
    fclose(configFile);
    return EXIT_SUCCESS;
}
```

**Вивід:**
```
Database Server: "192.168.1.1"
Port 0: 8001
Port 1: 8001
Port 2: 8002
```

## Поглиблений огляд

TOML був створений Томом Престоном-Вернером, співзасновником GitHub, як відповідь на обмеження, які він помітив у інших форматах файлів конфігурації. Його мета - бути прямолінійним та недвозначним, як для людей, так і для комп'ютерів, для читання та запису без необхідності складних правил парсингу. В екосистемі C TOML не є об'єктом першого класу, як це може бути у вищих мовах програмування, таких як Rust із його `serde_toml` або Python із `toml`, які мають бібліотеки з нативною підтримкою. Замість цього, розробникам на C потрібно покладатися на зовнішні бібліотеки, як `tomlc99`, але це типово, враховуючи наголос C на мінімалізмі та продуктивності.

Хоча TOML хвалять за його ясність, при виборі формату файлу конфігурації важливо враховувати потреби проєкту. У сценаріях, що вимагають більш складних структур або взаємодії з веб-API, JSON або навіть YAML можуть пропонувати краще рішення, незважаючи на їх збільшену складність. TOML відзначається у конфігураціях, де читабельність та простота є першочерговими, а не обов'язково там, де потрібні найбільш передові структури даних.
