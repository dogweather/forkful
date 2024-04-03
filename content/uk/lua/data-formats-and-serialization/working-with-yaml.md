---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:24.863853-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : Lua \u043D\u0435 \u043C\u0430\u0454 \u0432\u0431\u0443\u0434\u043E\u0432\u0430\
  \u043D\u043E\u0457 \u043F\u0456\u0434\u0442\u0440\u0438\u043C\u043A\u0438 \u0434\
  \u043B\u044F YAML, \u0430\u043B\u0435 \u0432\u0438 \u043C\u043E\u0436\u0435\u0442\
  \u0435 \u043F\u0440\u0430\u0446\u044E\u0432\u0430\u0442\u0438 \u0437 YAML-\u0444\
  \u0430\u0439\u043B\u0430\u043C\u0438, \u0432\u0438\u043A\u043E\u0440\u0438\u0441\
  \u0442\u043E\u0432\u0443\u044E\u0447\u0438 \u0441\u0442\u043E\u0440\u043E\u043D\u043D\
  \u0456 \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0438, \u0442\u0430\
  \u043A\u0456 \u044F\u043A `lyaml`. \u0426\u044F\u2026"
lastmod: '2024-03-13T22:44:49.534410-06:00'
model: gpt-4-0125-preview
summary: "Lua \u043D\u0435 \u043C\u0430\u0454 \u0432\u0431\u0443\u0434\u043E\u0432\
  \u0430\u043D\u043E\u0457 \u043F\u0456\u0434\u0442\u0440\u0438\u043C\u043A\u0438\
  \ \u0434\u043B\u044F YAML, \u0430\u043B\u0435 \u0432\u0438 \u043C\u043E\u0436\u0435\
  \u0442\u0435 \u043F\u0440\u0430\u0446\u044E\u0432\u0430\u0442\u0438 \u0437 YAML-\u0444\
  \u0430\u0439\u043B\u0430\u043C\u0438, \u0432\u0438\u043A\u043E\u0440\u0438\u0441\
  \u0442\u043E\u0432\u0443\u044E\u0447\u0438 \u0441\u0442\u043E\u0440\u043E\u043D\u043D\
  \u0456 \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0438, \u0442\u0430\
  \u043A\u0456 \u044F\u043A `lyaml`."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 YAML"
weight: 41
---

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
