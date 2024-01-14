---
title:                "Bash: Отримання поточної дати"
programming_language: "Bash"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Чому

Збирання поточної дати є важливою частиною програмування у Bash. Він дозволяє отримувати і використовувати поточну дату в різних сценаріях, таких як реєстрація подій, робота з змінними та попередження про утилізацію ресурсів.

## Як

Для початку, використовуйте команду `date` для виведення поточної дати та часу у вигляді тексту. Наприклад:

```Bash
date
```

Ви отримаєте результат у вигляді "Ср, 9 чер 2021 10:49:12 +0300".

Також, ви можете скористатися функцією `date +%d-%m-%Y`, щоб отримати різні формати дати, наприклад:

```Bash
date +%d.%m.%Y
```

Ви отримаєте результат у вигляді "09.06.2021".

## Глибоке погруження

Через функцію `date`, ви можете підрахувати різноманітні значення, такі як дні, місяці та роки. Наприклад, якщо ви хочете отримати рік в поточній даті, використовуйте `%Y`, або `%D` для дня.

Наприклад, для виведення поточного року у форматі "Y", використовуйте наступну команду:

```Bash
date +%Y
```

Отриманий результат буде "2021".

Навіть більше, ви можете використовувати дату для зчитування різних датових змінних, таких як `%m` для місяца або `%T` для часу.

## Дивись також

- [Bash Date](https://www.linuxjournal.com/content/converting-and-formatting-dates-using-gnu-date) - стаття про переведення та форматування дат у Bash.
- [Date Command in Linux](https://www.howtoforge.com/tutorial/linux-date-command/) - практичний посібник з використання команди `date` в Linux.
- [Using the Date Command in Bash](https://www.baeldung.com/linux/use-date-command-bash) - розширене посібник з різними прикладами використання команди `date` у Bash.

## Ресурси

- [Команда `date` у Bash](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html) - офіційна документація про команду `date` у Bash.
- [Робота з датою та часом в Bash](https://www.brainbell.com/tutorials/Linux/WorkingWithDates&Times.htm) - корисний туторіал з базовими командами та прикладами для роботи з датою та часом у Bash.