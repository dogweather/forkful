---
title:                "Порівняння двох дат"
html_title:           "Clojure: Порівняння двох дат"
simple_title:         "Порівняння двох дат"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Що та Навіщо?

Порівняння двох дат - це операція, що визначає, яка з дат раніша, пізніша або чи вони однакові. Програмісти роблять це, щоб контролювати якісь події відносно часу.

## Як це робити:

```Fish Shell
set date1 (date -u -d '2022-01-01' +%s)
set date2 (date -u -d '2022-02-01' +%s)

if test $date1 -eq $date2 
    echo 'Dates are equal'
else if test $date1 -lt $date2
    echo 'Date1 is less than Date2'
else
    echo 'Date1 is greater than Date2'
end
```
У даному прикладі ми создаємо дві дати у Unix форматі і порівнюємо їх. 

## Занурення глибше:

1. *Історичний контекст*: Порівняння дат було важливим аспектом програмування від самого початку. Воно дозволяє слідкувати за подіями в часі та контролювати процеси, що відбуваються в системі.

2. *Альтернативи*: Існують альтернативні способи порівняння дат в Fish Shell, наприклад, використовуючи `date` у комбінації з `math`.

```Fish Shell
set seconds_per_day 86400
set date1 (date -u -d '2022-01-01' +%s)
set date2 (date -u -d '2022-02-01' +%s)
set diff_days (math "$date2 - $date1")/86400
echo $diff_days
```

3. *Подробиці реалізації*: Fish Shell використовує Unix час (кількість секунд з 1 січня 1970 року) для порівняння дат. Цей метод є універсальним і дозволяє легко порівнювати дати.

## Див. також:

1. Детально про [Fish Shell](https://fishshell.com/docs/current/index.html)
3. Unix [час](https://en.wikipedia.org/wiki/Unix_time)