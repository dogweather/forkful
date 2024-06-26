---
date: 2024-01-26 04:09:27.758342-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0412 PowerShell \u0432\u0438 \u043C\u043E\u0436\u0435\u0442\u0435 \u043D\u0430\
  \u043B\u0430\u0433\u043E\u0434\u0436\u0443\u0432\u0430\u0442\u0438 \u0441\u043A\u0440\
  \u0438\u043F\u0442\u0438, \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\
  \u0432\u0443\u044E\u0447\u0438 \u0432\u0431\u0443\u0434\u043E\u0432\u0430\u043D\u0435\
  \ \u0441\u0435\u0440\u0435\u0434\u043E\u0432\u0438\u0449\u0435 PowerShell Integrated\
  \ Scripting Environment (ISE) \u0430\u0431\u043E\u2026"
lastmod: '2024-03-13T22:44:49.657581-06:00'
model: gpt-4-0125-preview
summary: "\u0412 PowerShell \u0432\u0438 \u043C\u043E\u0436\u0435\u0442\u0435 \u043D\
  \u0430\u043B\u0430\u0433\u043E\u0434\u0436\u0443\u0432\u0430\u0442\u0438 \u0441\u043A\
  \u0440\u0438\u043F\u0442\u0438, \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\
  \u043E\u0432\u0443\u044E\u0447\u0438 \u0432\u0431\u0443\u0434\u043E\u0432\u0430\u043D\
  \u0435 \u0441\u0435\u0440\u0435\u0434\u043E\u0432\u0438\u0449\u0435 PowerShell Integrated\
  \ Scripting Environment (ISE) \u0430\u0431\u043E Visual Studio Code (VS Code) \u0437\
  \ \u0440\u043E\u0437\u0448\u0438\u0440\u0435\u043D\u043D\u044F\u043C PowerShell."
title: "\u0412\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u043D\u043D\u044F \u0434\
  \u0435\u0431\u0430\u0433\u0435\u0440\u0430"
weight: 35
---

## Як це зробити:
В PowerShell ви можете налагоджувати скрипти, використовуючи вбудоване середовище PowerShell Integrated Scripting Environment (ISE) або Visual Studio Code (VS Code) з розширенням PowerShell. Ось як використовувати точки зупину в обох:

### PowerShell ISE:
```PowerShell
# Встановити точку зупину на певному рядку
Set-PSBreakpoint -Script .\MyScript.ps1 -Line 5

# Запустити ваш скрипт звичайним способом
.\MyScript.ps1

# Коли скрипт досягає точки зупину, ви можете інспектувати змінні
$myVariable

# Продовжити виконання
Continue
```

### Visual Studio Code:
```PowerShell
# Відкрити ваш PowerShell скрипт у VS Code.
# Клацнути ліворуч від номера рядка, щоб встановити точку зупину.
# Розпочати налагодження, натиснувши F5 або клацнувши "Start Debugging".

# VS Code зупинить виконання на вашій точці зупину.
# Використовуйте панель налагодження, щоб спостерігати за змінними, інспектувати стек викликів і контролювати потік.
```

Налагодження в обох середовищах дозволяє вам крокувати всередину (F11), крокувати з обходом (F10) та крокувати назовні (Shift+F11) під час налагодження.

## Поглиблений огляд
Історично налагодження у PowerShell було дещо незграбним; це вимагало багато рядків `Write-Host` для виведення станів змінних або класичний метод спроб та помилок. З появою PowerShell ISE, і нещодавно, VS Code з його багатими можливостями налагодження, налагодження PowerShell стало майже настільки інтуїтивно зрозумілим, як у повноцінних мовах програмування.

Альтернативи рідним інструментам налагодження PowerShell включають сторонні інструменти, як-от PowerGUI, або використання потужних IDE, як Visual Studio із плагіном PowerShell.

При впровадженні дебагера враховуйте область дії скрипта, особливо при роботі з dot-sourced скриптами або модулями. Точки зупину можуть бути засновані на умовах, змінах змінних або рядках, що дозволяє точно контролювати процес налагодження.

Більше того, з переходом на PowerShell Core (кросплатформенний PowerShell), налагодження в значній мірі перейшло до VS Code, який забезпечує послідовний досвід на різних платформах.

## Дивіться також
Для додаткової інформації про налагодження в PowerShell:
- [about_Debuggers](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_Debuggers)
