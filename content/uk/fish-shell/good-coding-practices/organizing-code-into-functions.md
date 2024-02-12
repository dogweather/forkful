---
title:                "Організація коду у функції"
aliases: - /uk/fish-shell/organizing-code-into-functions.md
date:                  2024-01-28T23:02:01.509562-07:00
model:                 gpt-4-0125-preview
simple_title:         "Організація коду у функції"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/organizing-code-into-functions.md"
changelog:
  - 2024-01-28, dogweather, reviewed and added links
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та Чому?
Організовувати код у функції означає групувати частинки сценарію для виконання конкретних завдань. Ми робимо це, тому що це робить код легшим для читання, тестування та повторного використання — нікому не хочеться боротися з болотом спагеті коду.

## Як це зробити:
У Fish ви пишете функцію за допомогою ключового слова `function`, даєте їй назву і закінчуєте `end`. Ось простий приклад:

```fish
function hello
    echo "Hello, World!"
end

hello
```

Вивід:
```
Hello, World!
```

Тепер зробімо так, щоб вона вітала користувача:

```fish
function greet
    set user (whoami)
    echo "Hey there, $user!"
end

greet
```

Вивід:
```
Hey there, ваш_логін!
```

Щоб зберегти її для наступних сесій, використовуйте `funcsave greet`.

## Поглиблено
Функції у Fish Shell схожі на міні-скрипти — ви можете додати туди майже все. Історично концепція функцій у скриптінгу командної оболонки зберегла безліч годин на повторне набирання та відлагодження. На відміну від мов програмування, як-от Python, функції командної оболонки більш про зручність, ніж про структуру.

Деякі оболонки, як-от Bash, використовують `function` або просто фігурні дужки. Fish тримається за `function ... end` — чітко та зрозуміло. Усередині функцій Fish ви отримуєте всі фішки: параметри, локальні змінні з `set -l`, і ви навіть можете визначити функцію всередині іншої функції.

Вам не знадобиться значення `return`, оскільки Fish не акцентує на цьому; виходом вашої функції є її результат. І якщо ви хочете мати постійні функції, доступні для майбутніх сесій, пам'ятайте про `funcsave`.

## Дивіться також

- Навчальний посібник по функціям у fish: [https://fishshell.com/docs/current/tutorial.html#tut_functions](https://fishshell.com/docs/current/tutorial.html#functions)

### Команди для роботи з функціями

- [function](https://fishshell.com/docs/current/cmds/function.html) — Створити функцію
- [functions](https://fishshell.com/docs/current/cmds/functions.html) — Роздрукувати або стерти функції
- [funcsave](https://fishshell.com/docs/current/cmds/funcsave.html) — Зберегти визначення функції в директорію автозавантаження користувача
- [funced](https://fishshell.com/docs/current/cmds/funced.html) — Інтерактивно редагувати функцію
