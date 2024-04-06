---
date: 2024-01-20 17:28:31.922931-07:00
description: "How to: (\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\
  \u0438:) \u041F\u0440\u0430\u0446\u044F \u0437 \u0434\u0430\u0442\u0430\u043C\u0438\
  \ \u2014 \u0446\u0435 \u0441\u0442\u0430\u043D\u0434\u0430\u0440\u0442\u043D\u0438\
  \u0439 \u043A\u0443\u0441\u043E\u043A \u0440\u043E\u0431\u043E\u0442\u0438 \u0434\
  \u043B\u044F \u0431\u0430\u0448. \u0412\u0456\u0434 \u0434\u043D\u0456\u0432 Unix,\
  \ `date` \u0431\u0443\u0432 \u043E\u0441\u043D\u043E\u0432\u043D\u0438\u043C \u0456\
  \u043D\u0441\u0442\u0440\u0443\u043C\u0435\u043D\u0442\u043E\u043C. \u0410\u043B\
  \u044C\u0442\u0435\u0440\u043D\u0430\u0442\u0438\u0432\u0438 \u0432\u043A\u043B\u044E\
  \u0447\u0430\u044E\u0442\u044C\u2026"
lastmod: '2024-04-05T22:51:02.623951-06:00'
model: gpt-4-1106-preview
summary: "(\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438:)\
  \ \u041F\u0440\u0430\u0446\u044F \u0437 \u0434\u0430\u0442\u0430\u043C\u0438 \u2014\
  \ \u0446\u0435 \u0441\u0442\u0430\u043D\u0434\u0430\u0440\u0442\u043D\u0438\u0439\
  \ \u043A\u0443\u0441\u043E\u043A \u0440\u043E\u0431\u043E\u0442\u0438 \u0434\u043B\
  \u044F \u0431\u0430\u0448."
title: "\u041E\u0431\u0447\u0438\u0441\u043B\u0435\u043D\u043D\u044F \u0434\u0430\u0442\
  \u0438 \u0443 \u043C\u0430\u0439\u0431\u0443\u0442\u043D\u044C\u043E\u043C\u0443\
  \ \u0447\u0438 \u043C\u0438\u043D\u0443\u043B\u043E\u043C\u0443"
weight: 26
---

## How to: (Як це зробити:)
```Bash
# Add days to the current date
date -d "+5 days" '+%Y-%m-%d'
```

```Bash
# Subtract days from the current date
date -d "-5 days" '+%Y-%m-%d'
```

```Bash
# Use a specific date as a starting point
date -d "2023-03-15 +10 days" '+%Y-%m-%d'
```

```Bash
# Output examples
2023-03-20 # Five days added to the current date
2023-03-10 # Five days subtracted from the current date
2023-03-25 # Ten days added to March 15, 2023
```

## Deep Dive (Поглиблений Аналіз):
Праця з датами — це стандартний кусок роботи для баш. Від днів Unix, `date` був основним інструментом. Альтернативи включають `GNU date`, `datetime` модулі в Python, або використання `dateutils` в Linux.

Налаштування баш скриптів для роботи з датами важливе для автоматизації та логування. Більшість систем підтримують цю команду без додаткових інсталяцій.

## See Also (Дивись також):
- GNU Coreutils Manual: https://www.gnu.org/software/coreutils/manual/coreutils.html#date-invocation
- `dateutils` documentation: http://www.fresse.org/dateutils/
- Bash script examples for date manipulation: https://mywiki.wooledge.org/BashFAQ/081
