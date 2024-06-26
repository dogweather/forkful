---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:25.291251-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0421\u0442\u0430\u043D\u0434\u0430\u0440\u0442\u043D\u0438\u0439 Prelude \u0432\
  \ Haskell \u043D\u0430\u0434\u0430\u0454 \u0431\u0430\u0437\u043E\u0432\u0443 \u043F\
  \u0456\u0434\u0442\u0440\u0438\u043C\u043A\u0443 \u0434\u043B\u044F \u0437\u0430\
  \u043F\u0438\u0441\u0443 \u0443 \u0444\u0430\u0439\u043B\u0438 \u0437\u0430 \u0434\
  \u043E\u043F\u043E\u043C\u043E\u0433\u043E\u044E \u0444\u0443\u043D\u043A\u0446\u0456\
  \u0439 `writeFile` \u0442\u0430 `appendFile` \u0437 \u043C\u043E\u0434\u0443\u043B\
  \u044F\u2026"
lastmod: '2024-03-13T22:44:49.391471-06:00'
model: gpt-4-0125-preview
summary: "\u0421\u0442\u0430\u043D\u0434\u0430\u0440\u0442\u043D\u0438\u0439 Prelude\
  \ \u0432 Haskell \u043D\u0430\u0434\u0430\u0454 \u0431\u0430\u0437\u043E\u0432\u0443\
  \ \u043F\u0456\u0434\u0442\u0440\u0438\u043C\u043A\u0443 \u0434\u043B\u044F \u0437\
  \u0430\u043F\u0438\u0441\u0443 \u0443 \u0444\u0430\u0439\u043B\u0438 \u0437\u0430\
  \ \u0434\u043E\u043F\u043E\u043C\u043E\u0433\u043E\u044E \u0444\u0443\u043D\u043A\
  \u0446\u0456\u0439 `writeFile` \u0442\u0430 `appendFile` \u0437 \u043C\u043E\u0434\
  \u0443\u043B\u044F `System.IO`."
title: "\u041D\u0430\u043F\u0438\u0441\u0430\u043D\u043D\u044F \u0442\u0435\u043A\u0441\
  \u0442\u043E\u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0443"
weight: 24
---

## Як це зробити:
Стандартний Prelude в Haskell надає базову підтримку для запису у файли за допомогою функцій `writeFile` та `appendFile` з модуля `System.IO`. Ось базовий приклад створення нового файлу (або перезапису існуючого) та потім додавання тексту до файлу.

```haskell
import System.IO

-- Запис у файл, перезаписуючи його, якщо він існує
main :: IO ()
main = do
  writeFile "example.txt" "Це перший рядок.\n"
  appendFile "example.txt" "Це другий рядок.\n"
```

Коли ви виконаєте цю програму, вона створить (або очистить) `example.txt` і запише "Це перший рядок." за яким слідує "Це другий рядок." на наступному рядку.

Для більш складної обробки файлів, програмісти на Haskell часто звертаються до пакету `text` для ефективної обробки рядків та пакету `bytestring` для роботи з двійковими даними. Ось як використовувати пакет `text` для вводу/виводу файлів:

Спочатку вам потрібно додати `text` до залежностей вашого проекту. Тоді ви можете використовувати його так:

```haskell
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- Запис у файл за допомогою пакету text
main :: IO ()
main = do
  let content = T.pack "Використання пакету text для кращої продуктивності.\n"
  TIO.writeFile "textExample.txt" content
  TIO.appendFile "textExample.txt" $ T.pack "Додавання другого рядка.\n"
```

У цьому уривку, `T.pack` конвертує звичайний `String` у тип `Text`, що є ефективнішим. `TIO.writeFile` та `TIO.appendFile` - це еквіваленти з пакету `text` для запису та додавання до файлів відповідно.

Виконання цього коду призведе до створення файлу під назвою `textExample.txt` з двома рядками тексту, що демонструє можливості створення та додавання за допомогою передової бібліотеки `text` для кращої продуктивності та здатності обробляти текст у форматі Unicode.
