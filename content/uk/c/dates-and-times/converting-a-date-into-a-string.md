---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:54:35.674859-07:00
description: "\u041F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F\
  \ \u0434\u0430\u0442\u0438 \u0443 \u0440\u044F\u0434\u043E\u043A \u0432 \u043C\u043E\
  \u0432\u0456 \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u0443\u0432\u0430\u043D\u043D\
  \u044F C \u0432\u043A\u043B\u044E\u0447\u0430\u0454 \u043F\u0435\u0440\u0435\u043A\
  \u043B\u0430\u0434 \u0441\u0442\u0440\u0443\u043A\u0442\u0443\u0440\u0438 \u0434\
  \u0430\u0442\u0438 \u0430\u0431\u043E \u0447\u0430\u0441\u043E\u0432\u043E\u0457\
  \ \u043C\u0456\u0442\u043A\u0438 \u0432 \u0444\u043E\u0440\u043C\u0430\u0442, \u0437\
  \u0440\u0443\u0447\u043D\u0438\u0439 \u0434\u043B\u044F \u0441\u043F\u0440\u0438\
  \u0439\u043D\u044F\u0442\u0442\u044F \u043B\u044E\u0434\u0438\u043D\u043E\u044E\
  . \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438\u2026"
lastmod: '2024-03-13T22:44:50.167874-06:00'
model: gpt-4-0125-preview
summary: "\u041F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F\
  \ \u0434\u0430\u0442\u0438 \u0443 \u0440\u044F\u0434\u043E\u043A \u0432 \u043C\u043E\
  \u0432\u0456 \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u0443\u0432\u0430\u043D\u043D\
  \u044F C \u0432\u043A\u043B\u044E\u0447\u0430\u0454 \u043F\u0435\u0440\u0435\u043A\
  \u043B\u0430\u0434 \u0441\u0442\u0440\u0443\u043A\u0442\u0443\u0440\u0438 \u0434\
  \u0430\u0442\u0438 \u0430\u0431\u043E \u0447\u0430\u0441\u043E\u0432\u043E\u0457\
  \ \u043C\u0456\u0442\u043A\u0438 \u0432 \u0444\u043E\u0440\u043C\u0430\u0442, \u0437\
  \u0440\u0443\u0447\u043D\u0438\u0439 \u0434\u043B\u044F \u0441\u043F\u0440\u0438\
  \u0439\u043D\u044F\u0442\u0442\u044F \u043B\u044E\u0434\u0438\u043D\u043E\u044E\
  . \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438\u2026"
title: "\u041F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0434\
  \u0430\u0442\u0438 \u0432 \u0440\u044F\u0434\u043E\u043A"
---

{{< edit_this_page >}}

## Що та чому?

Перетворення дати у рядок в мові програмування C включає переклад структури дати або часової мітки в формат, зручний для сприйняття людиною. Програмісти часто виконують це завдання, щоб відображати дати в журналах, інтерфейсах користувачів або при зберіганні дат в текстових форматах, наприклад, JSON або CSV.

## Як це робити:

Для цієї мети зазвичай використовується функція `strftime` з бібліотеки `<time.h>`. Вона дозволяє форматувати дату та час різними способами, вказуючи специфікатори формату. Ось швидкий приклад:

```c
#include <stdio.h>
#include <time.h>

int main() {
    char dateStr[100];
    time_t now = time(NULL);
    struct tm *ptm = localtime(&now);

    // Перетворення дати та часу на рядок (наприклад, "Wed Jun 30 21:49:08 2021")
    strftime(dateStr, sizeof(dateStr), "%a %b %d %H:%M:%S %Y", ptm);
    
    printf("Поточна дата та час: %s\n", dateStr);
    return 0;
}
```

Приклад виводу може виглядати так:

```
Поточна дата та час: Wed Jun 30 21:49:08 2021
```

Ви можете налаштувати формат, змінивши специфікатори формату, передані в `strftime`. Наприклад, щоб отримати дату в форматі `YYYY-MM-DD`, ви б використали `"%Y-%m-%d"`.

## Поглиблений огляд

Функція `strftime` та бібліотека `<time.h>` є частиною стандартної бібліотеки мови C, яка датується оригінальним стандартом ANSI C (C89/C90). Хоча цей підхід простий і підтримується на багатьох платформах, в порівнянні з сучасними мовами програмування, які пропонують більш інтуїтивно зрозумілі бібліотеки для роботи з датами та часом, він може здатися низькорівневим і громіздким.

Важливо зазначити, що, хоча функції часу стандартної бібліотеки C широко підтримуються і відносно прості у використанні, вони не мають деяких більш складних можливостей маніпулювання часовими поясами та інтернаціоналізації, які знаходяться в бібліотеках новіших мов або у сторонніх бібліотеках С, таких як Міжнародні компоненти для Unicode (ICU).

Проте, здатність функції `strftime` до налаштування та широка підтримка платформ роблять її надійним та корисним інструментом для перетворення дати в рядок в мові С. Програмісти, які прийшли з мов з більш високорівневими бібліотеками для роботи з датою та часом, можуть потребувати адаптації до її низькорівневої природи, але виявлять її надзвичайно потужною та гнучкою для форматування дат та часу для різноманітних застосувань.
