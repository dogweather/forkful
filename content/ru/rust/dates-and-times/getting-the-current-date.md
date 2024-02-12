---
title:                "Получение текущей даты"
date:                  2024-01-28T23:58:45.712003-07:00
model:                 gpt-4-0125-preview
simple_title:         "Получение текущей даты"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/rust/getting-the-current-date.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?

В программировании получение текущей даты помогает отслеживать события или фиксировать данные. Это практично для таких вещей, как проставление временных меток, расписаний или просто знание о том, когда что-то произошло.

## Как это сделать:

```Rust
use chrono::{DateTime, Local};

fn main() {
    let now: DateTime<Local> = Local::now();
    println!("{}", now.format("%Y-%m-%d %H:%M:%S"));
}
```

Вывод:
```
2023-04-05 14:20:35
```

## Подробный анализ

Rust, язык системного программирования, ориентированный на безопасность и производительность, не обладает функционалом для работы с датой и временем в своей стандартной библиотеке. Вместо этого сообщество создает крейты — термин Rust для библиотек или пакетов. Один из выдающихся крейтов — это `chrono`.

`chrono` предлагает богатые возможности для работы с датой и временем. Более того, он учитывает часовые пояса, что не является тривиальной задачей. Крейт использует данные о часовых поясах от `IANA` (Internet Assigned Numbers Authority) для корректного представления местных дат и времени.

Существуют и альтернативные крейты, такие как `time`, но они могут иметь другие интерфейсы или функции. Для более простых задач `time` может быть быстрее и иметь меньше зависимостей.

Получение местного времени включает в себя системные вызовы для взаимодействия с операционной системой. Точность и детализация могут варьироваться и зависят от системы и её конфигурации.

Детали реализации также заслуживают упоминания о философии дизайна. Rust отдает предпочтение явности. Таким образом, когда вы получаете текущее время, вы явно выбираете местное время вместо UTC, осведомленность о часовых поясах и так далее — минимизируя неожиданности и способствуя целенаправленности в коде.

## Смотрите также:

- Документация к крейту `chrono` в Rust: https://docs.rs/chrono/
- Документация к крейту `time` в Rust: https://docs.rs/time/
- База данных временных зон `IANA`: https://www.iana.org/time-zones