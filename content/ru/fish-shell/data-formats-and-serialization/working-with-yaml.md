---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:03.164541-07:00
description: "\u041A\u0430\u043A: ."
lastmod: '2024-03-13T22:44:45.876803-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 YAML"
weight: 41
---

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
