---
title:    "Fish Shell: Пошук та заміна тексту"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Чому

Не залежно від того, чи ви програміст, системний адміністратор або просто любите швидко і ефективно працювати з текстом, знання заміни та пошуку тексту є корисними для кожного. Використання командного рядка для цих завдань може значно збільшити продуктивність та зробити рутинну роботу швидшою.

## Як це зробити

```Fish Shell``` має потужну вбудовану утиліту заміни за допомогою команди ```sed```. Дозволяється використовувати регулярні вирази та замінювати на вказаний текст. 

Наприклад, якщо ми бажаємо замінити усі входження слова "рік" на слово "день" у документі ```example.txt```, ми можемо використовувати наступну команду:

```fish
sed -i 's/рік/день/g' example.txt
```

Ця команда виконує заміну на місці (в документі) та зберігає змінений файл. Використання параметру ```g``` вказує замінювати всі входження, а не тільки перше.

## Поглиблене дослідження

Більш детальну інформацію про синтаксис команди ```sed``` можна знайти у документації Fish Shell або наступних ресурсах:

- [Документація Fish Shell](https://fishshell.com/docs/current/index.html)
- [Регулярні вирази у сed](https://www.gnu.org/software/sed/manual/html_node/Regular-Expressions.html)
- [Підручник зі сed для початківців](https://waterprogramming.wordpress.com/2015/04/03/beginners-guide-to-sed-in-the-terminal/)

## Дивись також

Цей блог-пост допоміг вам зрозуміти, як використовувати заміну та пошук тексту в Fish Shell. Якщо ви зацікавилися подібними темами, можете ознайомитися з наступними статтями:

- [Основи командного рядка для початківців](https://ru.hexlet.io/courses/cli-basics/lessons/command-line/theory_unit)
- [Маніпуляції з текстом в командному рядку](https://linuxconfig.org/manipulating-text-with-sed-and-awk)
- [Регулярні вирази для пошуку та заміни тексту](https://www.digitalocean.com/community/tutorials/an-introduction-to-regular-expressions)