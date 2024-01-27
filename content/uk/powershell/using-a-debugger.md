---
title:                "Використання дебагера"
date:                  2024-01-26T04:09:27.758342-07:00
model:                 gpt-4-0125-preview
simple_title:         "Використання дебагера"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/using-a-debugger.md"
---

{{< edit_this_page >}}

## Що і чому?
Використання дебагера передбачає встановлення точок зупину, прохід через ваш код крок за кроком, спостереження за змінними та інспектування стану вашої програми під час її виконання. Це змінює правила гри для програмістів, оскільки це дозволяє виявляти помилки та допомагає нам зрозуміти, що насправді робить наш код.

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
- [Документація PowerShell про налагодження](https://docs.microsoft.com/en-us/powershell/scripting/debugging/debugging-in-powershell?view=powershell-7.2)
- [Розширення PowerShell для Visual Studio Code](https://marketplace.visualstudio.com/items?itemName=ms-vscode.PowerShell)