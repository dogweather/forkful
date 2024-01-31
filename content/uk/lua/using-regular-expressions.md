---
title:                "Використання регулярних виразів"
date:                  2024-01-19
html_title:           "Bash: Використання регулярних виразів"
simple_title:         "Використання регулярних виразів"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/lua/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Що це таке та навіщо?

Регулярні вирази - це шаблони для пошуку та маніпуляції текстом. Програмісти використовують їх для валідації, розбору, та заміни даних, здійснюючи це швидко та ефективно.

## Як це зробити:

```Lua
local text = "Контакт: +380 (93) 123-45-67"
local pattern = "%d%d%d%d%d%d%d%d%d%d%d"
local phoneNumber = string.match(text, pattern)
print(phoneNumber)  -- Виводить: 380931234567
```

```Lua
local emails = "пошта1@приклад.com, невалідна-пошта, пошта2@приклад.ua"
for email in string.gmatch(emails, "[a-zA-Z0-9._%-%+]+@[a-zA-Z0-9.%-]+%.%a%a+") do
    print(email)  -- Виводить: пошта1@приклад.com та пошта2@приклад.ua
end
```

## Поглиблений аналіз

Регулярні вирази базуються на фундаментальних концепціях теорії формальних мов та були розроблені в 1950-х роках. У Lua, регулярні вирази представлені як частина бібліотеки рядків і мають власний синтаксис, відмінний від Perl-сумісних regex-бібліотек. Як альтернативу, можна використовувати зовнішні бібліотеки, наприклад, LuaPCRE. Деталі реалізації включають патерни символів, набори символів, згруповані вирази та модифікатори.

## Читати також

- Онлайн документація Lua: https://www.lua.org/manual/5.4/manual.html#6.4.1
- Lua Users Wiki - Patterns: http://lua-users.org/wiki/PatternsTutorial
- Introduction to Lua pattern matching: https://www.codeproject.com/Articles/9099/The-30-Minute-Regex-Tutorial
- Використання LuaPCRE для повноцінних регулярних виразів: https://luapcre.luaforge.net/
