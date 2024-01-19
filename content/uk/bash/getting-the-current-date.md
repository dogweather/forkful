---
title:                "Отримання поточної дати"
html_title:           "Bash: Отримання поточної дати"
simple_title:         "Отримання поточної дати"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Що і чому?
Отримання поточної дати – це процес, коли програма визначає сьогоднішню дату. Програмісти роблять це для журналювання, штампів часу та для обробки дати і часу.

## Як це зробити:
Використовуйте команду `date` в Bash для отримання поточної дати та часу. Ось приклад:

```Bash
$ date
```

Виведення:

```Bash
Tue May 18 14:25:11 EEST 2021
```
Бажаєте вивести дату в іншому форматі? Використовуйте такий приклад:

```Bash
$ date +"%d/%m/%Y"
```

Виведення:

```Bash
18/05/2021
```

## Поглиблений вхід
- Історичний контекст: Команда `date` випливає з UNIX, де її були включили в BSD Unix в 1979 році.
- Альтернативи: Інші команди, як `printf`, також можуть використовуватися для виводу дати.
- Деталі реалізації: Команда `date` конвертує поточний час (виконання), представлений як кількість секунд, що минали з `UNIX Epoch (1970-01-01 00:00:00 UTC)`, в більш зручний формат.

## Дивись також:
- Основи Bash: [bOtskOOl](https://www.botskool.com/geeks/bash-basic-commands)
- Більше про команду `date`: [Cyberciti](https://www.cyberciti.biz/faq/unix-linux-getting-current-date-in-bash-ksh-shell-script/)
- Інформація про форматування дати й часу bash: [GNU](https://www.gnu.org/software/bash/manual/bash.html#Date-Manipulation)