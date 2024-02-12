---
title:                "Робота з YAML"
aliases:
- /uk/lua/working-with-yaml.md
date:                  2024-02-03T19:26:24.863853-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/lua/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і Чому?

YAML, скорочення від "YAML Ain't Markup Language" (YAML - це не мова розмітки), є стандартом серіалізації даних, який сприймається людиною та часто використовується для файлів конфігурації та обміну даними між мовами програмування. Програмісти використовують YAML завдяки його простоті та зручності для сприйняття, що робить його переважним вибором для налаштувань, різноманітних конфігурацій програм або вмісту, який повинен бути доступний для редагування не програмістами.

## Як це зробити:

Lua не має вбудованої підтримки для YAML, але ви можете працювати з YAML-файлами, використовуючи сторонні бібліотеки, такі як `lyaml`. Ця бібліотека дозволяє кодувати та декодувати дані YAML за допомогою Lua. Спочатку вам потрібно встановити `lyaml` через LuaRocks, менеджер пакетів Lua:

```bash
luarocks install lyaml
```

### Декодування YAML:

Припустимо, у вас є наступний YAML-вміст у файлі під назвою `config.yaml`:

```yaml
database:
  host: localhost
  port: 3306
  username: user
  password: pass
```

Ви можете декодувати цей YAML-файл у Lua-таблицю за допомогою наступного коду:

```lua
local yaml = require('lyaml')
local file = io.open("config.yaml", "r")
local content = file:read("*all")
file:close()

local data = yaml.load(content)
for k,v in pairs(data.database) do
  print(k .. ": " .. v)
end
```

Коли ви запустите цей скрипт, він має вивести:

```output
host: localhost
port: 3306
username: user
password: pass
```

### Кодування YAML:

Щоб закодувати Lua-таблиці у формат YAML, ви використовуєте функцію `dump`, яку надає `lyaml`. Припустімо, ви хочете створити YAML-представлення наступної Lua-таблиці:

```lua
local data = {
  website = {
    name = "Example",
    owner = "Jane Doe",
    metadata = {
      creation_date = "2023-01-01",
      tags = {"blog", "personal", "lua"}
    }
  }
}

local yaml = require('lyaml')
local yaml_data = yaml.dump({data})
print(yaml_data)
```

Вивід YAML буде:

```yaml
- website:
    metadata:
      creation_date: '2023-01-01'
      tags: [blog, personal, lua]
    name: Example
    owner: Jane Doe
```

Дотримуючись цих шаблонів, програмісти Lua можуть ефективно управляти даними YAML для різноманітних застосувань. Ці операції з YAML є критично важливими для розробки універсальних програм Lua, які взаємодіють гладко з іншими частинами системи або безпосередньо з іншими системами.
