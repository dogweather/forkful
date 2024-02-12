---
title:                "Работа с YAML"
aliases:
- ru/bash/working-with-yaml.md
date:                  2024-01-29T00:04:44.751106-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/bash/working-with-yaml.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
YAML — это не язык разметки (YAML Ain't Markup Language) — стандарт сериализации данных, предназначенный для чтения человеком. Программисты используют его для файлов конфигурации, хранения данных и межпроцессного обмена сообщениями из-за его простоты и читаемости.

## Как:
Вот простой пример чтения файла YAML с использованием Bash.

Дан `config.yaml`:
```yaml
database:
  host: localhost
  port: 3306
  username: user
  password: pass123
```

Используйте этот скрипт для чтения YAML и вывода хоста базы данных:

```Bash
#!/bin/bash
value=$(grep 'host:' config.yaml | awk '{ print $2 }')
echo "Хост базы данных: ${value}"
```

Пример вывода:
```
Хост базы данных: localhost
```

## Глубокое погружение
YAML, созданный в 2001 году, является более дружественной к человеку альтернативой XML или JSON. Он широко используется в облачных сервисах, развертывании приложений и инструментах devops. Хотя Bash изначально не имеет средств разбора YAML, инструменты, такие как `yq`, и разбор с помощью `awk` или `grep`, могут справиться с этой задачей. Однако, для сложного разбора может понадобиться специализированное средство для работы с YAML.

## Смотрите также
- Официальный сайт YAML: https://yaml.org
- `yq`, командная строка обработки YAML: https://github.com/kislyuk/yq
- Обсуждение разбора YAML в Bash: https://stackoverflow.com/questions/5014632/how-can-i-parse-a-yaml-file-from-a-linux-shell-script
