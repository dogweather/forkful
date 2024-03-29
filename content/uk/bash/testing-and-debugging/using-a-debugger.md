---
date: 2024-01-26 03:48:23.588612-07:00
description: "\u0412\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u043D\u043D\u044F\
  \ \u0434\u0435\u0431\u0430\u0433\u0435\u0440\u0430 \u0432 Bash \u043E\u0437\u043D\
  \u0430\u0447\u0430\u0454 \u0437\u0430\u0441\u0442\u043E\u0441\u0443\u0432\u0430\u043D\
  \u043D\u044F \u0456\u043D\u0441\u0442\u0440\u0443\u043C\u0435\u043D\u0442\u0456\u0432\
  \ \u0434\u043B\u044F \u0442\u0435\u0441\u0442\u0443\u0432\u0430\u043D\u043D\u044F\
  \ \u0442\u0430 \u043F\u043E\u0448\u0443\u043A\u0443 \u043F\u0440\u043E\u0431\u043B\
  \u0435\u043C \u0443 \u0432\u0430\u0448\u0438\u0445 \u0441\u043A\u0440\u0438\u043F\
  \u0442\u0430\u0445, \u044F\u043A-\u043E\u0442 \u0432\u0438\u044F\u0432\u043B\u0435\
  \u043D\u043D\u044F \u043F\u043E\u043C\u0438\u043B\u043E\u043A, \u0449\u043E \u0437\
  \u043C\u0443\u0448\u0443\u044E\u0442\u044C \u0432\u0430\u0448\u2026"
lastmod: '2024-03-13T22:44:49.585217-06:00'
model: gpt-4-0125-preview
summary: "\u0412\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u043D\u043D\u044F\
  \ \u0434\u0435\u0431\u0430\u0433\u0435\u0440\u0430 \u0432 Bash \u043E\u0437\u043D\
  \u0430\u0447\u0430\u0454 \u0437\u0430\u0441\u0442\u043E\u0441\u0443\u0432\u0430\u043D\
  \u043D\u044F \u0456\u043D\u0441\u0442\u0440\u0443\u043C\u0435\u043D\u0442\u0456\u0432\
  \ \u0434\u043B\u044F \u0442\u0435\u0441\u0442\u0443\u0432\u0430\u043D\u043D\u044F\
  \ \u0442\u0430 \u043F\u043E\u0448\u0443\u043A\u0443 \u043F\u0440\u043E\u0431\u043B\
  \u0435\u043C \u0443 \u0432\u0430\u0448\u0438\u0445 \u0441\u043A\u0440\u0438\u043F\
  \u0442\u0430\u0445, \u044F\u043A-\u043E\u0442 \u0432\u0438\u044F\u0432\u043B\u0435\
  \u043D\u043D\u044F \u043F\u043E\u043C\u0438\u043B\u043E\u043A, \u0449\u043E \u0437\
  \u043C\u0443\u0448\u0443\u044E\u0442\u044C \u0432\u0430\u0448\u2026"
title: "\u0412\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u043D\u043D\u044F \u0434\
  \u0435\u0431\u0430\u0433\u0435\u0440\u0430"
---

{{< edit_this_page >}}

## Що і Чому?
Використання дебагера в Bash означає застосування інструментів для тестування та пошуку проблем у ваших скриптах, як-от виявлення помилок, що змушують ваш код збоїти або непомітно працювати неправильно. Програмісти роблять це, бо є значно розумніше виявляти помилки, перш ніж вони нароблять лиха в робочому середовищі.

## Як:
Bash не має вбудованого дебагера, як деякі інші мови, але ви можете використовувати вбудовані команди, такі як `set -x`, для відстеження того, що відбувається. Або, для покращення, є `bashdb`, відповідний дебагер для крокового проходження через ваш код. Ось короткий огляд:

```Bash
# Використання set -x для дебагу
set -x
echo "Початок дебагінгу"
my_var="Вітаємо, Світ Дебагінгу!"
echo $my_var
set +x

# Використання bashdb
# Встановіть bashdb за допомогою вашого менеджера пакетів, наприклад, apt, yum, brew.
# Дебаг скрипта під назвою my_script.sh:
bashdb my_script.sh
```

Вивід при виконанні з `set -x`:
```Bash
+ echo 'Початок дебагінгу'
Початок дебагінгу
+ my_var='Вітаємо, Світ Дебагінгу!'
+ echo 'Вітаємо, Світ Дебагінгу!'
Вітаємо, Світ Дебагінгу!
+ set +x
```

## Поглиблений аналіз
Історично, дебагінг Bash скриптів означав розставляння у коді команд `echo`. Але потім з'явилась команда `set -x`, яка дала нам можливість зазирнути у виконання коду без ручного виведення результатів. І для тих, хто прагне більшого контролю, з'явився дебагер `bashdb`, натхненний дебагером gdb для C/C++.

Що стосується альтернатив, то окрім команд `set` (`-x`, `-v`, `-e`), є інші варіанти: перенаправлення виводу до файлу для аналізу або використання зовнішніх інструментів, таких як ShellCheck, для статичного аналізу.

З точки зору реалізації, `set -x` легко застосовувати; це рідний варіант Bash, який друкує команди та їх аргументи під час виконання. З іншого боку, `bashdb` дозволяє крокувати через код, встановлювати точки зупинки та оцінювати вирази - речі, що дають вам боротись з більш непомітними помилками.

## Див. також
- Проект Bash Debugger: http://bashdb.sourceforge.net/
- "Pro Bash Programming" Кріса Джонсона і Джаянта Варми для поглибленого скриптінгу.
- ShellCheck для статичного аналізу: https://www.shellcheck.net/
