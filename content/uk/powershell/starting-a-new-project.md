---
title:                "Початок нового проєкту"
html_title:           "Elm: Початок нового проєкту"
simple_title:         "Початок нового проєкту"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Що і чому?

Розпочати новий проект – це процес створення нової програми чи системи з нуля. Програмісти роблять це, щоб вирішити конкретну проблему або зробити життя людей простішим та комфортнішим.

## Як це зробити:

Ось як ви можете використовувати PowerShell для розпочатку нового проекту:

```PowerShell
New-Item —ItemType Directory —Path "C:\MyNewProject"
Set-Location "C:\MyNewProject"
```
Це створить нову директорію для вашого проекту та перенаправить вас туди.

Вивід:

```PowerShell 
 Directory: C:\
 Mode                LastWriteTime         Length Name
 ----                -------------         ------ ----
 d-----        3/14/2023  2:00 PM                MyNewProject
```
## Глибше занурення: 

PowerShell - це мова сценаріїв, що була розроблена Microsoft у 2006 році як жодна її преемниця. Її імена та способи працювати були не лише створені, але й адаптовані до потреб спільноти.

Що стосується альтернатив, можна використовувати Bash чи Python для створення нових проектів. Настільки багато, наскільки їх можна завбачити, обираються все ж таки згідно з конкретними потребами проекту та комфортності використання конкретного програміста.

Деталі реалізації включають ініціалізацію робочого простору проекту, створення структурних папок, конфігурацію середовища та другі подібні аспекти.

## Дивіться також:

1. Офіційна документація PowerShell: [https://docs.microsoft.com/uk-ua/powershell/](https://docs.microsoft.com/uk-ua/powershell/)
2. Навчальний курс по PowerShell від Microsoft: [https://docs.microsoft.com/uk-ua/learn/modules/powershell-basics/](https://docs.microsoft.com/uk-ua/learn/modules/powershell-basics/)