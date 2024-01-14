---
title:                "Fish Shell: Обчислення дати в майбутньому або минулому."
simple_title:         "Обчислення дати в майбутньому або минулому."
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Чому

Немає нічого легшого, ніж відправитися в подорож у часі за допомогою Fish Shell. Іноді нам потрібно обчислити дату у майбутньому або минулому для планування або розрахунків. Це може бути важко, якщо робити це вручну, але з Fish Shell це стає надзвичайно простим і швидким процесом.

## Як

Використовуючи усі можливості Fish Shell, ми можемо легко розрахувати дату у майбутньому або минулому. Ось приклад коду для обчислення дати через 5 днів в майбутньому:

```
Fish Shell: 
set -l future_date (date -f '+%Y-%m-%d' (math (date -f '+%s') + (math 5 * 86400)))
echo $future_date
```

Виходячи з вищезазначеного коду, ми отримаємо дату у майбутньому у форматі "YYYY-MM-DD". Можна також змінити кількість днів, додавши або віднімаючи іншу кількість секунд.

Також, ви можете обчислити дату в минулому, змінивши "+" на "-":

```
Fish Shell:
set -l past_date (date -f '+%Y-%m-%d' (math (date -f '+%s') - (math 5 * 86400)))
echo $past_date
```

## Глибокий пір

Для тих, хто хоче докладніше розібратися з обчисленням дат, ще корисне було б дізнатися про різні формати дати та часу та їхні значення. Наприклад, на кожну дату можна додати або відняти певну кількість секунд (datetime), хвилин (minutetime), годин (hourtime), днів (daytime), тижнів (weektime), місяців (monthtime) або років (yeartime). Крім того, можна змінити вихідний формат дати, використавши "-f" параметр для date.

## Дивіться також

- [Fish Shell documentation](https://fishshell.com/docs/current/index.html)
- [Date and Time manipulation in Fish Shell](https://dev.to/jmflannery/how-to-manipulate-date-and-time-in-fish-shell-16a8)
- [More about manipulating dates in Fish Shell](https://github.com/fish-shell/fish-shell/issues/3492#issuecomment-294576720)