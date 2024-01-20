---
title:                "Робота з yaml"
html_title:           "Lua: Робота з yaml"
simple_title:         "Робота з yaml"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/lua/working-with-yaml.md"
---

{{< edit_this_page >}}

## Що & Чому?
Робота з YAML - це процес створення, редагування та читання даних у структурованому форматі, який називається "YAML" (або "YAML Ain't Markup Language"). Програмісти використовують YAML для збереження та передачі даних у зручному для читання форматі. Це допомагає спростити процес розробки програм та спільної роботи над проектами.

## Як це зробити:
```Lua 
-- Створення даних у форматі YAML
local data = {
  ["ім'я"] = "Олексій",
  ["вік"] = 25,
  ["хобі"] = {"подорожі", "програмування", "спорт"}
}

-- Запис даних у файл
local yaml = require("yaml")
local output = yaml.dump(data)
file = io.open("my_data.yaml", "w")
file:write(output)
file:close()

-- Читання даних з файлу
local input = io.open("my_data.yaml"):read("*all")
local decoded = yaml.load(input)

-- Виведення даних на екран
print("Привіт, мене звати " .. decoded["ім'я"] .. " і мені " .. decoded["вік"] .. " років.")
print("Одне з моїх улюблених хобі - " .. decoded["хобі"][1] .. ".")

-- Вихід:
-- Привіт, мене звати Олексій і мені 25 років.
-- Одне з моїх улюблених хобі - подорожі.
```

## Глибокий занурення:
YAML був створений у 2001 році як альтернатива XML та JSON форматам для збереження та передачі даних. Він має читабельний синтаксис, який допомагає виконувати зміни у файлах за допомогою простих текстових редакторів або засобів контролю версій, таких як Git. У Lua існує кілька бібліотек для роботи з YAML, таких як lua-yaml та lyaml.

## Дивись також:
- [YAML.org](https://yaml.org/) - офіційний сайт YAML
- [LYAML](https://github.com/gvvaughan/lyaml) - ще одна бібліотека для роботи з YAML у Lua