---
title:    "Bash: Обчислення дати у майбутньому або минулому"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Чому

Кодування важливий навик в сучасному світі, оскільки він дає змогу розвивати логічне мислення та використовувати ІТ-інструменти для автоматизації рутинних завдань. Розрахунок дати в майбутньому або минулому може бути корисним для планування подій або виконання історичних досліджень. Таким чином, вивчення цих навичок може бути корисним в багатьох сферах життя.

## Як

Для розрахунку дати в майбутньому або минулому використовуються різні методи та інструменти, але найпоширенішим та доступним є командний рядок Bash. Використовуючи цей метод, ми можемо швидко та просто отримати потрібну дату, використовуючи простий синтаксис та зручні функції.

```Bash
# Розрахунок дати в майбутньому
future_date=$(date -d "+7 days" +"%d/%m/%Y")
echo "Наступна дата через 7 днів: " $future_date

# Розрахунок дати в минулому
past_date=$(date -d "-1 month" +"%d/%m/%Y")
echo "Дата минулого місяця: " $past_date
```

Вивід:

Наступна дата через 7 днів: 01/05/2021
Дата минулого місяця: 01/03/2021

## Глибше дослідження

Щоб розуміти, як працюють ці команди, детальніше розглянемо їх. Команда `date` використовується для відображення часу та дати відповідно до формату, вказаного у флагу `-d` (дати). Ми можемо використовувати різні флаги, які визначають, на скільки днів або місяців необхідно змістити дату.

Форматування дати та часу виконується за допомогою флага `+"%d/%m/%Y"`, де `%d` відображає день місяця, `%m` - місяць, а `%Y` - рік.

Також, для більш точних розрахунків, ми можемо використовувати функції `timedelta` та `getdate` з пакету `dateutils`. Ці функції дають змогу зміщувати дату на кілька тижнів, місяців або років, а також розраховувати різницю між двома датами.

## Дивись також

- [10 Bash-команд, які потрібно знати](https://lifehacker.com/ten-simple-commands-every-linux-user-should-know-1207461511)
- [Документація по команд