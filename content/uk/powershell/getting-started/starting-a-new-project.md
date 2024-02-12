---
title:                "Починаємо новий проект"
aliases:
- /uk/powershell/starting-a-new-project.md
date:                  2024-01-20T18:05:03.848356-07:00
model:                 gpt-4-1106-preview
simple_title:         "Починаємо новий проект"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why? (Що та Чому?)
Започаткування нового проекту - це створення базового каркасу для майбутнього коду. Програмісти роблять це, щоб мати організований старт і виставити правильну структуру з самого початку.

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
