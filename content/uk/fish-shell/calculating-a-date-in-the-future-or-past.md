---
title:    "Fish Shell: Обчислення дати у майбутньому або минулому"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Чому

Іноді у житті нам потрібно знати дату у майбутньому або минулому. Наприклад, для планування подій або розрахунку термінів. У цих випадках можна скористатися програмуванням в розширеній оболонці Fish для швидкого і точного розрахунку потрібної дати. В цій статті ми розглянемо як саме це зробити.

## Як це зробити

Для початку, нам потрібно встановити Fish Shell на нашому комп'ютері. Далі, необхідно створити скрипт з кодом для обчислення дати.

```Fish Shell
#!/usr/bin/env fish

set current_date (date +%Y-%m-%d)
echo "Сьогоднішня дата: $current_date"

set future_date (date -d "+7 days" +%Y-%m-%d)
echo "Дата через 7 днів: $future_date"

set past_date (date -d "-1 month" +%Y-%m-%d)
echo "Дата 1 місяць тому: $past_date"
```

Збережіть скрипт з розширенням "`.fish`" і запустіть його в оболонці Fish. В результаті ви отримаєте вивід з сьогоднішнім, майбутнім і минулим датами. 

## Глибоке занурення

Окрім стандартних опцій для розрахунку дат, оболонка Fish також має багато вбудованих функцій для роботи з датами. Наприклад, ви можете отримати поточну дату і час, використовуючи команду "`.clock`", або порівняти дати за допомогою функції "`.date`". Також, у Fish є можливість працювати зі стандартними форматами дат, такими як "RFC 2822" або "ISO 8601". Більш детальну інформацію про ці можливості можна знайти в [документації] (https://fishshell.com/docs/current/cmds/date.html) оболонки Fish.

## Дивись також

- [Diving into Fish Shell] (https://macpaw.com/how-to/use-fish-shell-on-mac)
- [Fish Shell Cookbook] (https://fishshell.com/docs/current/cookbook.html)
- [Рекурсивні функції у Fish Shell] (https://samleo8.github.io/fish-shell-recursive-functions)
- [Підвищуємо продуктивність за допомогою Fish Shell] (https://medium.com/@tusharsadhwani/improving-productivity-with-fish-shell-44eea06c3d56)