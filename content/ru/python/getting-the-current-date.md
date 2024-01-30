---
title:                "Получение текущей даты"
date:                  2024-01-28T23:58:45.784295-07:00
model:                 gpt-4-0125-preview
simple_title:         "Получение текущей даты"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/python/getting-the-current-date.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Получение текущей даты в Python означает извлечение живой даты из системы, на которой он работает. Программисты делают это для логирования, временных меток или когда требуется текущая дата для пользовательского интерфейса или отчёта.

## Как это сделать:
Используйте модуль `datetime`. Это просто:

```Python
from datetime import datetime

# Получить текущую дату
current_date = datetime.now().date()

# Вывести её
print(current_date)
```

Пример вывода может выглядеть так:

```
2023-04-12
```

Примечание: Вывод зависит от дня, когда вы запускаете код. Очевидно.

## Погружение в детали
Модуль `datetime` не претерпел существенных изменений в последних версиях Python. Он является частью стандартной библиотеки Python – незамысловатого набора инструментов для работы с датами и временем. Альтернативы? Конечно, есть `time`, но он более примитивен. Для серьёзной работы мир обращается к `dateutil` и `arrow`, но для получения только сегодняшней даты? Оставайтесь с `datetime`.

Под капотом `datetime.now()` ловит текущий момент в соответствии с настройками времени вашего компьютера. Чтобы учитывать часовой пояс, вы бы использовали, например, `datetime.now(timezone.utc)`. Исторически работа с часовыми поясами была головной болью, поэтому всегда учитывайте местоположение и переход на летнее время, если это важно.

Для получения быстрой даты без временной метки – например, при создании файла с сегодняшней датой в его названии – `datetime.now().date()` даёт вам именно это: объект даты, содержащий год, месяц и день.

## Смотрите также
- Официальная документация Python по `datetime`: https://docs.python.org/3/library/datetime.html
- `arrow` для более сложной обработки даты/времени: https://arrow.readthedocs.io
- `dateutil`, потому что часовые пояса: https://dateutil.readthedocs.io
- Настройки времени вашего скромного ПК, потому что, ну, Python смотрит туда в первую очередь.