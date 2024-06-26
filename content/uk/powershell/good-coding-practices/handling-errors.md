---
date: 2024-01-26 00:56:55.676626-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : PowerShell \u0437\u0440\u043E\u0431\u0438\u0432 \u0434\u043E\u0432\u0433\u0438\
  \u0439 \u0448\u043B\u044F\u0445 \u0437 \u0447\u0430\u0441\u0443 \u0441\u0432\u043E\
  \u0433\u043E \u043F\u043E\u0447\u0430\u0442\u043A\u0443 \u044F\u043A Monad. \u041E\
  \u0431\u0440\u043E\u0431\u043A\u0430 \u043F\u043E\u043C\u0438\u043B\u043E\u043A\
  \ \u0441\u0442\u0430\u043B\u0430 \u0431\u0456\u043B\u044C\u0448 \u043D\u0430\u0434\
  \u0456\u0439\u043D\u043E\u044E \u0437 \u0447\u0430\u0441\u043E\u043C, \u043F\u0440\
  \u043E\u043F\u043E\u043D\u0443\u044E\u0447\u0438 \u0444\u0443\u043D\u043A\u0446\u0456\
  \u0457, \u043F\u043E\u0434\u0456\u0431\u043D\u0456 \u0434\u043E\u2026"
lastmod: '2024-04-05T21:53:49.786731-06:00'
model: gpt-4-1106-preview
summary: "PowerShell \u0437\u0440\u043E\u0431\u0438\u0432 \u0434\u043E\u0432\u0433\
  \u0438\u0439 \u0448\u043B\u044F\u0445 \u0437 \u0447\u0430\u0441\u0443 \u0441\u0432\
  \u043E\u0433\u043E \u043F\u043E\u0447\u0430\u0442\u043A\u0443 \u044F\u043A Monad."
title: "\u041E\u0431\u0440\u043E\u0431\u043A\u0430 \u043F\u043E\u043C\u0438\u043B\u043E\
  \u043A"
weight: 16
---

## Як це зробити:
```PowerShell
# Базовий Try-Catch для обробки винятків
try {
    # Код, який може спричинити помилку
    $result = 1 / 0
} catch {
    # Що робити, якщо сталася помилка
    Write-Host "Ой, сталася помилка: $_"
}

# Виведення згенерованого повідомлення про помилку
try {
    Get-Item "nonexistentfile.txt" -ErrorAction Stop
} catch {
    Write-Host "Файл не можна знайти."
}

# Використання змінної $Error для перевірки останньої помилки
```

## Поглиблено
PowerShell зробив довгий шлях з часу свого початку як Monad. Обробка помилок стала більш надійною з часом, пропонуючи функції, подібні до інших мов програмування. Синтаксис `try-catch-finally` - це один з таких прийомів, запозичених з мов, як C#. До його появи скрипт-розробники значною мірою покладались на перевірку умов і використання автоматичної змінної `$Error`.

У PowerShell також є два основні типи помилок: термінуючі та нетермінуючі. Термінуючі помилки зупинять скрипт, якщо їх не перехоплено у блоку `try-catch`, тоді як нетермінуючі не зупинять, якщо ви не вкажете `-ErrorAction Stop`. Це розрізнення критичне, оскільки воно дозволяє точно контролювати обробку помилок, вирішуючи, чи помилка дійсно вимагає зупинки всього скрипта або може бути просто зареєстрована та ігнорована.

Обробка помилок у PowerShell також дозволяє використовувати блок `finally`, який виконується завжди - незалежно від того, чи сталася помилка чи ні. Він чудово підходить для завдань з очистки.

Коли ви глибоко занурені у писання скриптів, ви також можете обробляти конкретні типи винятків, що надає вам ще більший контроль.

Крім того, є стара добра параметри `-ErrorVariable` для захоплення помилок без викидання винятку. А змінна `$?` повідомляє вам, чи була остання операція успішною. Вони корисні інструменти, хоча трохи менш чистіші, ніж надійний `try-catch`.

## Дивіться також
- [about_Try_Catch_Finally](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_try_catch_finally?view=powershell-7.2)
