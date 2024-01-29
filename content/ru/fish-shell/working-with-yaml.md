---
title:                "Работа с YAML"
date:                  2024-01-29T00:05:03.164541-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с YAML"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/fish-shell/working-with-yaml.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

YAML, "YAML Ain't Markup Language" (YAML — это не язык разметки), представляет собой удобный для человека стандарт сериализации данных, который по нотации превосходит табличные и языки разметки для файлов конфигурации и обмена данными. Программисты используют его за его простоту и читаемость в файлах конфигурации, манифестах развертывания и более сложных структурах данных.

## Как:

### Чтение YAML Конфига
```Fish Shell
# Предполагая, что 'config.yaml' содержит:
# name: Fishy
# occupation: Shell

set config (yaml2json < config.yaml | jq -r '.name, .occupation')
echo $config
# Вывод: Fishy Shell
```

### Запись в YAML Файл
```Fish Shell
# Используя 'yq', портативный командный процессор YAML
echo -e "name: Nemo\ncolor: Orange" > fish.yaml

# Добавление нового ключа
yq e '.friends += ["Dory"]' -i fish.yaml

cat fish.yaml
# Вывод:
# name: Nemo
# color: Orange
# friends:
# - Dory
```

## Подробнее

YAML появился в начале 2000-х как упрощение XML и с тех пор стал стандартом для файлов конфигурации в индустрии программного обеспечения. Его минимальный синтаксис является и благом, и проклятием — легко читается, но сложно разбирать без библиотек. Альтернативы YAML включают JSON, XML и TOML, каждый из которых имеет свои компромиссы по использованию. В Fish Shell для манипуляций с YAML файлами обычно используются `yq` и `yaml2json`, так как в Fish Shell нет встроенного разбора YAML.

## Смотрите также

- Официальный сайт YAML: https://yaml.org
- Руководство `jq`: https://stedolan.github.io/jq/manual/
- Репозиторий и документация `yq`: https://github.com/mikefarah/yq
