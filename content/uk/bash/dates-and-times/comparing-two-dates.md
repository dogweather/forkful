---
date: 2024-01-20 17:32:41.285063-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0414\u043B\u044F \u043F\u043E\u0440\u0456\u0432\u043D\u044F\u043D\u043D\u044F\
  \ \u0434\u0430\u0442 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\
  \u0443\u044E\u0442\u044C `date` \u0442\u0430 \u0456\u043D\u0448\u0456 \u043A\u043E\
  \u043C\u0430\u043D\u0434\u0438. \u041E\u0441\u044C \u043A\u0456\u043B\u044C\u043A\
  \u0430 \u043F\u0440\u0438\u043A\u043B\u0430\u0434\u0456\u0432."
lastmod: '2024-03-13T22:44:49.598929-06:00'
model: gpt-4-1106-preview
summary: "\u0414\u043B\u044F \u043F\u043E\u0440\u0456\u0432\u043D\u044F\u043D\u043D\
  \u044F \u0434\u0430\u0442 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\
  \u0432\u0443\u044E\u0442\u044C `date` \u0442\u0430 \u0456\u043D\u0448\u0456 \u043A\
  \u043E\u043C\u0430\u043D\u0434\u0438."
title: "\u041F\u043E\u0440\u0456\u0432\u043D\u044F\u043D\u043D\u044F \u0434\u0432\u043E\
  \u0445 \u0434\u0430\u0442"
weight: 27
---

## Як це зробити:
Для порівняння дат використовують `date` та інші команди. Ось кілька прикладів:

```Bash
# Отримати поточну дату у форматі YYYY-MM-DD
current_date=$(date '+%Y-%m-%d')

# Порівняти з іншою датою
other_date="2023-04-01"
if [[ "$current_date" > "$other_date" ]]; then
  echo "Поточна дата після 2023-04-01"
else
  echo "Поточна дата до або в 2023-04-01"
fi

# Використання date для перетворення в секунди (epoch time) для порівняння
current_epoch=$(date '+%s')
other_epoch=$(date -d "$other_date" '+%s')

if [[ "$current_epoch" -gt "$other_epoch" ]]; then
  echo "Поточна дата після 2023-04-01"
else
  echo "Поточна дата до або в 2023-04-01"
fi
```

Вивід буде відрізнятися в залежності від поточної дати та дати для порівняння.

## Поглиблений розбір:
Перші версії Bash не мали вбудованих засобів для порівняння дат, але з часом додали `date` для роботи з часом. Порівняння може бути прямим (у форматі рядка) или через секунди з 1970 року (epoch time). Альтернатива `date` — це `[[ ]]` для порівняння рядків (працює, якщо формат правильний). Зовнішні інструменти, такі як `awk` чи `perl`, також можуть бути використані для порівняння дат.

## Дивіться також:
- Чоловік (`man`) сторінки `date` для детального опису (`man date` в терміналі).
- [Bash Guide for Beginners](https://tldp.org/LDP/Bash-Beginners-Guide/html/) для базових знань про скрипти Bash.
- [Advanced Bash-Scripting Guide](https://www.tldp.org/LDP/abs/html/) для більш детального розуміння скриптів.
- [Stack Overflow](https://stackoverflow.com/questions/tagged/bash+date) для питань та відповідей щодо порівняння дат у Bash.
