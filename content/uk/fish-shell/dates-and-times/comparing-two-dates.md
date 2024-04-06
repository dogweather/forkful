---
date: 2024-01-20 17:32:56.257811-07:00
description: "How to: (\u042F\u043A \u0440\u043E\u0431\u0438\u0442\u0438:) \u0423\
  \ \u043C\u0438\u043D\u0443\u043B\u043E\u043C\u0443 \u043F\u043E\u0440\u0456\u0432\
  \u043D\u044F\u043D\u043D\u044F \u0434\u0430\u0442 \u043C\u043E\u0433\u043B\u043E\
  \ \u0431\u0443\u0442\u0438 \u043A\u043B\u043E\u043F\u0456\u0442\u043A\u0438\u043C\
  \ \u0447\u0435\u0440\u0435\u0437 \u0440\u0456\u0437\u043D\u0456 \u0444\u043E\u0440\
  \u043C\u0430\u0442\u0438 \u0442\u0430 \u0447\u0430\u0441\u043E\u0432\u0456 \u0437\
  \u043E\u043D\u0438. \u0423 Fish Shell \u0432\u0438\u043A\u043E\u0440\u0438\u0441\
  \u0442\u043E\u0432\u0443\u044E\u0442\u044C \u043A\u043E\u043C\u0430\u043D\u0434\u0443\
  \ `date` \u0434\u043B\u044F\u2026"
lastmod: '2024-04-05T22:51:02.976900-06:00'
model: gpt-4-1106-preview
summary: "(\u042F\u043A \u0440\u043E\u0431\u0438\u0442\u0438:) \u0423 \u043C\u0438\
  \u043D\u0443\u043B\u043E\u043C\u0443 \u043F\u043E\u0440\u0456\u0432\u043D\u044F\u043D\
  \u043D\u044F \u0434\u0430\u0442 \u043C\u043E\u0433\u043B\u043E \u0431\u0443\u0442\
  \u0438 \u043A\u043B\u043E\u043F\u0456\u0442\u043A\u0438\u043C \u0447\u0435\u0440\
  \u0435\u0437 \u0440\u0456\u0437\u043D\u0456 \u0444\u043E\u0440\u043C\u0430\u0442\
  \u0438 \u0442\u0430 \u0447\u0430\u0441\u043E\u0432\u0456 \u0437\u043E\u043D\u0438\
  ."
title: "\u041F\u043E\u0440\u0456\u0432\u043D\u044F\u043D\u043D\u044F \u0434\u0432\u043E\
  \u0445 \u0434\u0430\u0442"
weight: 27
---

## How to: (Як робити:)
```fish
# Визначимо дати
set date1 (date -ud "2023-03-15" +%s)
set date2 (date -ud "2023-03-20" +%s)

# Порівняємо дати
if test $date1 -lt $date2
    echo "date1 раніше date2"
else if test $date1 -gt $date2
    echo "date1 пізніше date2"
else
    echo "date1 і date2 однакові"
end
```
Output:
```
date1 раніше date2
```

## Deep Dive (Поглиблений Розгляд)
У минулому порівняння дат могло бути клопітким через різні формати та часові зони. У Fish Shell використовують команду `date` для перетворення дати у секунди з початку епохи (Unix timestamp), що спрощує порівняння. Альтернативи включають скрипти на Perl чи Python. Деталі впровадження важливі, адже не всі системи мають однакові версії команди `date` і їх параметри можуть відрізнятися.

## See Also (Дивіться також)
- Руководство `date` command: https://fishshell.com/docs/current/cmds/date.html
- Fish Shell documentation: https://fishshell.com/docs/current/index.html
- Unix Timestamp converter: https://www.epochconverter.com/
