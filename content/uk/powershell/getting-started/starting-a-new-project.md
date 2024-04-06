---
date: 2024-01-20 18:05:03.848356-07:00
description: "How to: (\u042F\u043A \u0440\u043E\u0431\u0438\u0442\u0438:) \u0417\u0430\
  \u043F\u043E\u0447\u0430\u0442\u043A\u0443\u0432\u0430\u043D\u043D\u044F \u043F\u0440\
  \u043E\u0435\u043A\u0442\u0443 \u0443 PowerShell \u0437\u0430\u0433\u0430\u043B\u043E\
  \u043C \u043D\u0435 \u0432\u0456\u0434\u0440\u0456\u0437\u043D\u044F\u0454\u0442\
  \u044C\u0441\u044F \u0432\u0456\u0434 \u0456\u043D\u0448\u0438\u0445 \u043C\u043E\
  \u0432: \u0432\u0438 \u0441\u0442\u0432\u043E\u0440\u044E\u0454\u0442\u0435 \u043F\
  \u0440\u0438\u0437\u043D\u0430\u0447\u0435\u043D\u0443 \u0441\u0442\u0440\u0443\u043A\
  \u0442\u0443\u0440\u0443 \u043F\u0430\u043F\u043E\u043A, \u0444\u0430\u0439\u043B\
  \u0438,\u2026"
lastmod: '2024-04-05T22:51:02.664315-06:00'
model: gpt-4-1106-preview
summary: "(\u042F\u043A \u0440\u043E\u0431\u0438\u0442\u0438:) \u0417\u0430\u043F\u043E\
  \u0447\u0430\u0442\u043A\u0443\u0432\u0430\u043D\u043D\u044F \u043F\u0440\u043E\u0435\
  \u043A\u0442\u0443 \u0443 PowerShell \u0437\u0430\u0433\u0430\u043B\u043E\u043C\
  \ \u043D\u0435 \u0432\u0456\u0434\u0440\u0456\u0437\u043D\u044F\u0454\u0442\u044C\
  \u0441\u044F \u0432\u0456\u0434 \u0456\u043D\u0448\u0438\u0445 \u043C\u043E\u0432\
  ."
title: "\u041F\u043E\u0447\u0438\u043D\u0430\u0454\u043C\u043E \u043D\u043E\u0432\u0438\
  \u0439 \u043F\u0440\u043E\u0435\u043A\u0442"
weight: 1
---

## How to: (Як робити:)
```PowerShell
# Створення нового проекту в PowerShell
New-Item -Path 'C:\MyProject' -ItemType Directory
cd C:\MyProject

# Ініціалізація git репозиторія, якщо потрібно
git init

# Створення стандартних папок проекту
New-Item -Path '.\src', '.\test' -ItemType Directory

# Створення базового скрипта
New-Item -Path '.\src\MyScript.ps1' -ItemType File
"Not all those who wander are lost" > .\src\MyScript.ps1

# Вивід структури проекту
Get-ChildItem -Recurse
```

Приклад виводу:
```Shell
    Directory: C:\MyProject

Mode                LastWriteTime         Length Name
----                -------------         ------ ----
d-----        1/1/2023   12:00 AM                src
d-----        1/1/2023   12:00 AM                test
-a----        1/1/2023   12:01 AM              0 MyScript.ps1
```

## Deep Dive (Поглиблено)
Започаткування проекту у PowerShell загалом не відрізняється від інших мов: ви створюєте призначену структуру папок, файли, встановлюєте залежності. До речі, PowerShell був випущений від Microsoft як частина Windows Management Framework. Він дає можливість автоматизувати багато завдань, пов'язаних із управлінням системою.

Історично PowerShell використовував командний скрипт (.cmd) або пакетні (.bat) файли для автоматизації завдань. Зараз же, з PowerShell, можна працювати набагато вишуканіше, роблячи скрипти потужнішими та читабельнішими.

Альтернативами є інші мови скриптів, наприклад Python або Bash, які відомі своєю портативністю та широким спектром застосування.

Започаткування проекту може також включати налаштування віртуального середовища, що дозволяє вам управляти залежностями проекту окремо від вашої основної системи. PowerShell не має будованого менеджеру віртуального середовища, але є рішення, такі як VirtualEnv для Python.

## See Also (Додатково)
- [About PowerShell](https://docs.microsoft.com/powershell/scripting/overview) - офіційна документація PowerShell
- [Pro Git](https://git-scm.com/book/en/v2) - книга для поглибленого вивчення Git
- [PowerShell Gallery](https://www.powershellgallery.com/) - центральне сховище скриптів та модулів PowerShell
- [Windows Command Line Tools For Developers](https://devblogs.microsoft.com/commandline/) - блог Microsoft з корисною інформацією для розробників командного рядка
