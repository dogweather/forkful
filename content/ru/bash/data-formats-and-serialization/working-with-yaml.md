---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:44.751106-07:00
description: "\u041A\u0430\u043A: \u0412\u043E\u0442 \u043F\u0440\u043E\u0441\u0442\
  \u043E\u0439 \u043F\u0440\u0438\u043C\u0435\u0440 \u0447\u0442\u0435\u043D\u0438\
  \u044F \u0444\u0430\u0439\u043B\u0430 YAML \u0441 \u0438\u0441\u043F\u043E\u043B\
  \u044C\u0437\u043E\u0432\u0430\u043D\u0438\u0435\u043C Bash. \u0414\u0430\u043D\
  \ `config.yaml`."
lastmod: '2024-03-13T22:44:45.407098-06:00'
model: gpt-4-0125-preview
summary: "\u0412\u043E\u0442 \u043F\u0440\u043E\u0441\u0442\u043E\u0439 \u043F\u0440\
  \u0438\u043C\u0435\u0440 \u0447\u0442\u0435\u043D\u0438\u044F \u0444\u0430\u0439\
  \u043B\u0430 YAML \u0441 \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\
  \u043D\u0438\u0435\u043C Bash."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 YAML"
weight: 41
---

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
