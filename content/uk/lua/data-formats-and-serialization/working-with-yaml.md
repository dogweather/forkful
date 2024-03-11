---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:24.863853-07:00
description: "YAML, \u0441\u043A\u043E\u0440\u043E\u0447\u0435\u043D\u043D\u044F \u0432\
  \u0456\u0434 \"YAML Ain't Markup Language\" (YAML - \u0446\u0435 \u043D\u0435 \u043C\
  \u043E\u0432\u0430 \u0440\u043E\u0437\u043C\u0456\u0442\u043A\u0438), \u0454 \u0441\
  \u0442\u0430\u043D\u0434\u0430\u0440\u0442\u043E\u043C \u0441\u0435\u0440\u0456\u0430\
  \u043B\u0456\u0437\u0430\u0446\u0456\u0457 \u0434\u0430\u043D\u0438\u0445, \u044F\
  \u043A\u0438\u0439 \u0441\u043F\u0440\u0438\u0439\u043C\u0430\u0454\u0442\u044C\u0441\
  \u044F \u043B\u044E\u0434\u0438\u043D\u043E\u044E \u0442\u0430 \u0447\u0430\u0441\
  \u0442\u043E\u2026"
lastmod: '2024-03-11T00:14:23.401435-06:00'
model: gpt-4-0125-preview
summary: "YAML, \u0441\u043A\u043E\u0440\u043E\u0447\u0435\u043D\u043D\u044F \u0432\
  \u0456\u0434 \"YAML Ain't Markup Language\" (YAML - \u0446\u0435 \u043D\u0435 \u043C\
  \u043E\u0432\u0430 \u0440\u043E\u0437\u043C\u0456\u0442\u043A\u0438), \u0454 \u0441\
  \u0442\u0430\u043D\u0434\u0430\u0440\u0442\u043E\u043C \u0441\u0435\u0440\u0456\u0430\
  \u043B\u0456\u0437\u0430\u0446\u0456\u0457 \u0434\u0430\u043D\u0438\u0445, \u044F\
  \u043A\u0438\u0439 \u0441\u043F\u0440\u0438\u0439\u043C\u0430\u0454\u0442\u044C\u0441\
  \u044F \u043B\u044E\u0434\u0438\u043D\u043E\u044E \u0442\u0430 \u0447\u0430\u0441\
  \u0442\u043E\u2026"
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 YAML"
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
