---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:55:55.300539-07:00
description: "\u0420\u0430\u0441\u0447\u0435\u0442 \u0431\u0443\u0434\u0443\u0449\u0435\
  \u0439 \u0438\u043B\u0438 \u043F\u0440\u043E\u0448\u0435\u0434\u0448\u0435\u0439\
  \ \u0434\u0430\u0442\u044B \u0432\u043A\u043B\u044E\u0447\u0430\u0435\u0442 \u0432\
  \ \u0441\u0435\u0431\u044F \u043C\u0430\u043D\u0438\u043F\u0443\u043B\u044F\u0446\
  \u0438\u0438 \u0441 \u0434\u0430\u0442\u0430\u043C\u0438, \u0447\u0442\u043E\u0431\
  \u044B \u0443\u0437\u043D\u0430\u0442\u044C, \u043A\u0430\u043A\u043E\u0439 \u0434\
  \u0435\u043D\u044C \u0431\u044B\u043B \u0438\u043B\u0438 \u0431\u0443\u0434\u0435\
  \u0442. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B\
  \ \u0434\u0435\u043B\u0430\u044E\u0442 \u044D\u0442\u043E \u0434\u043B\u044F \u043F\
  \u043B\u0430\u043D\u0438\u0440\u043E\u0432\u0430\u043D\u0438\u044F,\u2026"
lastmod: '2024-03-13T22:44:45.864458-06:00'
model: gpt-4-0125-preview
summary: "\u0420\u0430\u0441\u0447\u0435\u0442 \u0431\u0443\u0434\u0443\u0449\u0435\
  \u0439 \u0438\u043B\u0438 \u043F\u0440\u043E\u0448\u0435\u0434\u0448\u0435\u0439\
  \ \u0434\u0430\u0442\u044B \u0432\u043A\u043B\u044E\u0447\u0430\u0435\u0442 \u0432\
  \ \u0441\u0435\u0431\u044F \u043C\u0430\u043D\u0438\u043F\u0443\u043B\u044F\u0446\
  \u0438\u0438 \u0441 \u0434\u0430\u0442\u0430\u043C\u0438, \u0447\u0442\u043E\u0431\
  \u044B \u0443\u0437\u043D\u0430\u0442\u044C, \u043A\u0430\u043A\u043E\u0439 \u0434\
  \u0435\u043D\u044C \u0431\u044B\u043B \u0438\u043B\u0438 \u0431\u0443\u0434\u0435\
  \u0442. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B\
  \ \u0434\u0435\u043B\u0430\u044E\u0442 \u044D\u0442\u043E \u0434\u043B\u044F \u043F\
  \u043B\u0430\u043D\u0438\u0440\u043E\u0432\u0430\u043D\u0438\u044F,\u2026"
title: "\u0420\u0430\u0441\u0447\u0435\u0442 \u0434\u0430\u0442\u044B \u0432 \u0431\
  \u0443\u0434\u0443\u0449\u0435\u043C \u0438\u043B\u0438 \u043F\u0440\u043E\u0448\
  \u043B\u043E\u043C"
---

{{< edit_this_page >}}

## Что и почему?
Расчет будущей или прошедшей даты включает в себя манипуляции с датами, чтобы узнать, какой день был или будет. Программисты делают это для планирования, напоминаний или определения сроков и дедлайнов.

## Как:
Вот крутой способ работать с датами в Fish Shell:

```Fish Shell
# Добавить дни к текущей дате
set -l days_to_add 10
date -d "+$days_to_add days"

# Пример вывода (варьируется в зависимости от текущей даты):
# Ср Мар 29 00:29:10 PDT 2023

# Вычесть дни из текущей даты
set -l days_to_subtract 10
date -d "-$days_to_subtract days"

# Пример вывода (снова, ваша дата может отличаться):
# Вс Мар 9 00:30:42 PDT 2023
```

## Подробнее
Fish не только о всплеске; он идет с историей. Оболочки вроде bash раньше были главными для расчетов с датами, обычно через GNU `date`. Fish, сохраняя простоту, использует похожий синтаксис, но может быть более удобным и понятным - отлично подходит как для начинающих пловцов, так и для опытных форелей.

Альтернативы для расчета дат включают в себя языки программирования, как Python, или использование `dateutils`. У каждого есть свои сильные стороны, хотя `dateutils` немного более неясный, а Python может быть излишним для простых задач. Внедрение в Fish является золотой серединой, с командой `date`, заимствующей стандарты UNIX - она почти везде установлена и гладко вписывается в настройки системного времени.

## Смотрите также
Для более подробной информации окунитесь в эти воды:
- [GNU Coreutils – Date](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html): Получите представление о том, как работает `date` изнутри.
- [Документация Fish Shell](https://fishshell.com/docs/current/index.html): Официальная документация, где вы можете узнать больше о Fish и его других командах.
- [StackOverflow: Арифметика дат](https://stackoverflow.com/questions/tagged/date-arithmetic): Посмотрите реальные проблемы и решения от сообщества.
