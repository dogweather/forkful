---
title:                "Bash: Порівняння двох дат."
programming_language: "Bash"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Чому

Нерідко програмісти зіткнуться з задачею порівняння двох дат в своїх проєктах. Це може бути корисно при роботі з базами даних, розробці програм, або для тестування. Наприклад, ви можете перевірити, чи дата була введена правильно або зробити умовну логіку, що залежить від дат.

## Як

Для порівняння двох дат в Bash використовується утиліта `date`. Спочатку нам потрібно встановити формат дати, використовуючи опцію `-d`. Потім ми можемо порівняти дати за допомогою умов `>=` або `<=`.

```Bash
# Встановлення формату дати
date1="2020-05-10"
date2="2020-05-12"

# Порівняння дат
if [[ $date1 > $date2 ]]; then
  echo "$date1 пізніше за $date2"
elif [[ $date1 < $date2 ]]; then
  echo "$date1 раніше за $date2"
else
  echo "Дати однакові"
fi
```

Вивід:

```
2020-05-10 раніше за 2020-05-12
```

Можна також використовувати опцію `-r` для порівняння дат з використанням їхніх таймстемпів. Наприклад:

```Bash
date1="2020-05-10"
date2=$(date "+%Y-%m-%d") # Поточна дата

if [[ $(date -d "$date1" +%s) -gt $(date -d "$date2" +%s) ]]; then
  echo "$date1 пізніше за поточну дату"
else
  echo "$date1 раніше або дорівнює поточній даті"
fi
```

Вивід:

```
2020-05-10 раніше або дорівнює поточній даті
```

## Глибоке занурення

Детальніше про роботу з датами в Bash можна дізнатися в [офіційній документації](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html#date-invocation). Там є багато корисних опцій для роботи з датами, таких як встановлення часових зон, врахування високосних років тощо.

## Дивіться також

- [Stack Overflow: Compare dates in Bash](https://stackoverflow.com/questions/18689457/comparing-two-dates-using-bash)
- [Linuxize: How to Compare Dates in Bash](https://linuxize.com/post/how-to-compare-dates-in-bash/)