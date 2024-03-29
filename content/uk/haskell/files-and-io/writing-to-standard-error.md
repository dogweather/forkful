---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:44.452810-07:00
description: "\u0417\u0430\u043F\u0438\u0441 \u0443 \u0441\u0442\u0430\u043D\u0434\
  \u0430\u0440\u0442\u043D\u0443 \u043F\u043E\u043C\u0438\u043B\u043A\u0443 (stderr)\
  \ \u043D\u0430 Haskell \u0434\u043E\u0437\u0432\u043E\u043B\u044F\u0454 \u043F\u0440\
  \u043E\u0433\u0440\u0430\u043C\u0430\u043C \u0440\u043E\u0437\u0440\u0456\u0437\u043D\
  \u044F\u0442\u0438 \u0441\u0432\u0456\u0439 \u0432\u0438\u0432\u0456\u0434 \u043C\
  \u0456\u0436 \u0437\u0432\u0438\u0447\u0430\u0439\u043D\u0438\u043C\u0438 \u0440\
  \u0435\u0437\u0443\u043B\u044C\u0442\u0430\u0442\u0430\u043C\u0438 \u0442\u0430\
  \ \u043F\u043E\u0432\u0456\u0434\u043E\u043C\u043B\u0435\u043D\u043D\u044F\u043C\
  \u0438 \u043F\u0440\u043E \u043F\u043E\u043C\u0438\u043B\u043A\u0438. \u0426\u0435\
  \u2026"
lastmod: '2024-03-13T22:44:49.387989-06:00'
model: gpt-4-0125-preview
summary: "\u0417\u0430\u043F\u0438\u0441 \u0443 \u0441\u0442\u0430\u043D\u0434\u0430\
  \u0440\u0442\u043D\u0443 \u043F\u043E\u043C\u0438\u043B\u043A\u0443 (stderr) \u043D\
  \u0430 Haskell \u0434\u043E\u0437\u0432\u043E\u043B\u044F\u0454 \u043F\u0440\u043E\
  \u0433\u0440\u0430\u043C\u0430\u043C \u0440\u043E\u0437\u0440\u0456\u0437\u043D\u044F\
  \u0442\u0438 \u0441\u0432\u0456\u0439 \u0432\u0438\u0432\u0456\u0434 \u043C\u0456\
  \u0436 \u0437\u0432\u0438\u0447\u0430\u0439\u043D\u0438\u043C\u0438 \u0440\u0435\
  \u0437\u0443\u043B\u044C\u0442\u0430\u0442\u0430\u043C\u0438 \u0442\u0430 \u043F\
  \u043E\u0432\u0456\u0434\u043E\u043C\u043B\u0435\u043D\u043D\u044F\u043C\u0438 \u043F\
  \u0440\u043E \u043F\u043E\u043C\u0438\u043B\u043A\u0438. \u0426\u0435\u2026"
title: "\u0417\u0430\u043F\u0438\u0441 \u0434\u043E \u0441\u0442\u0430\u043D\u0434\
  \u0430\u0440\u0442\u043D\u043E\u0457 \u043F\u043E\u043C\u0438\u043B\u043A\u0438"
---

{{< edit_this_page >}}

## Що і чому?
Запис у стандартну помилку (stderr) на Haskell дозволяє програмам розрізняти свій вивід між звичайними результатами та повідомленнями про помилки. Це критично важливо для сигналізації про проблеми та налагодження, не захаращуючи стандартний вивід (stdout), на якому часто знаходяться основні дані або результат програми.

## Як це зробити:
У Haskell запис у stderr виконується просто за допомогою модуля `System.IO` базової бібліотеки. Нижче наведено базовий приклад для демонстрації:

```haskell
import System.IO

main :: IO ()
main = do
  hPutStrLn stderr "Це повідомлення про помилку."
```

Вивід цієї програми у stderr буде:

```
Це повідомлення про помилку.
```

Якщо ви працюєте над більш складною аплікацією або якщо вам потрібний кращий контроль над логуванням (включаючи помилки), ви могли б обрати сторонню бібліотеку. Одним з популярних виборів є `monad-logger`, який інтегрується зі стилем програмування `mtl` на Haskell. Ось невеликий фрагмент, який використовує `monad-logger`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.Logger

main :: IO ()
main = runStderrLoggingT $ do
  logErrorN "Це повідомлення про помилку, використовуючи monad-logger."
```

При запуску версія `monad-logger` аналогічно виводить повідомлення про помилку, але воно оснащене більшим контекстом, як-от часові мітки або рівні логування, залежно від конфігурації:

```
[Помилка] Це повідомлення про помилку, використовуючи monad-logger.
```

Обидва методи служать для запису в stderr, і вибір в основному залежить від складності та потреб вашої аплікації.
