---
title:    "Python: Видобування підстроків"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Почему

В даний час програмування є одним з найбільш популярних напрямків у сучасному світі, і Python - одна з найуспішніших мов програмування. Важливо завжди вдосконалювати свої навички і вивчати нові функції, такі як витягування підрядків. Вони дозволяють обробляти дані більш ефективно і зрозуміти роботу програми за допомогою маніпулювання рядками. Тож давайте подивимося, як витягувати підрядки у Python і навчитися цьому корисному навику.

## Як це зробити

Для витягування підрядків у Python, ви можете використовувати функцію `slice()`, що дозволяє виділити певну частину рядка за допомогою індексів. Наприклад, якщо у нас є рядок `hello world`, і ми хочемо виділити лише слово `world`, ми можемо використати такий код:

```Python
s = 'hello world'
print(s[6:11])
# результат: world
```

У цьому коді `s[6:11]` означає, що ми вибираємо символи від індексу 6 (включно) до індексу 11 (не включно). Зверніть увагу, що перший символ має індекс 0, тому `s[6]` буде символ `w`.

Також можна використовувати негативні індекси, які дозволяють виділяти підрядки із кінця рядка. Наприклад, вираз `s[-5:-1]` у нашому прикладі даватиме результат `worl`.

Існує ще кілька способів витягувати підрядки, такі як використання кроку (`s[2::2]` - кожен другий символ починаючи з індексу 2), або еквівалентний синтаксис (`s[6:]` - з індексу 6 до кінця рядка). Для більш детальної інформації про всі можливості функції `slice()`, ви можете переглянути документацію [тут](https://docs.python.org/3/library/functions.html#slice).

## Глибокий погляд

Тепер давайте трохи краще зрозуміємо, як працює витягування підрядків у Python. Індексація починається з 0, тому символ `h` у нашому прикладі має індекс 0, `e` - 1, і так далі. Коли ми використовуємо двокрапку, ми вказуємо межі сегмента, який ми хочемо виділити. Тобто, вираз `s[6:11]` вибирає