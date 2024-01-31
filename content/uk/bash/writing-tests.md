---
title:                "Написання тестів"
date:                  2024-01-19
html_title:           "Arduino: Написання тестів"
simple_title:         "Написання тестів"

category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/bash/writing-tests.md"
---

{{< edit_this_page >}}

## Що це таке та навіщо?
Тести дозволяють перевіряти код на коректність. Програмісти пишуть тести для автоматизації перевірки функцій та скриптів, знижуючи ризик помилок при розробці.

## Як це робити:
```Bash
# Приклад простого тесту для функції:
sum() {
  echo $(($1 + $2))
}

# Тест функції sum:
result=$(sum 2 3)
expected_result=5

if [ "$result" -eq "$expected_result" ]; then
  echo "Тест пройшов"
else
  echo "Тест не пройшов: очікувалось $expected_result, отримано $result"
fi
```
```Bash
Тест пройшов
```

## Поглиблене вивчення:
Історично, Bash-тестування було простим, але з часом з'явились розширені інструменти як bash_unit, shunit2. Альтернативи, як Python's PyTest чи JavaScript's Jest, призначені для більш комплексних завдань та мов. Для Bash, рекомендується використовувати [ -перевірки ] і [[ розширені перевірки ]].

## Дивіться також:
- bash_unit: https://github.com/pgrange/bash_unit
- shunit2: https://github.com/kward/shunit2
- Advanced Bash-Scripting Guide: https://www.tldp.org/LDP/abs/html/
