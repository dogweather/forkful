---
title:                "Написання тестів"
html_title:           "Bash: Написання тестів"
simple_title:         "Написання тестів"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/bash/writing-tests.md"
---

{{< edit_this_page >}}

## Чому

Написання тестів є важливою частиною процесу розробки програмного забезпечення. Це дозволяє перевірити, чи працює програма так, як очікується, і запобігає появі несподіваних помилок в майбутньому.

## Як

```Bash
#!/bin/bash
# Код для тестування функції знаходження середнього значення

# оголошення тестових даних
numbers=(10 9 8 7 6 5)

# функція для знаходження середнього значення
average() {
  local sum=0
  for num in "${@}"; do
    ((sum+=num))
  done
  local avg=$((sum/${#numbers[@]}))
  echo "Середнє значення: $avg"
}

# виклик функції
average "${numbers[@]}"
```

Вивід: Середнє значення: 7

## Глибока погрузка

Написання коректних тестів є важливою задачею для забезпечення якості вашого програмного забезпечення. Деякі корисні поради для написання тестів:

- Створюйте тести для кожної функції в програмі
- Використовуйте функції assert для перевірки правильності результатів
- Дайте назви тестам, які відображають їх ціль та очікуваний результат

## Дивіться також

- [Bash Unit Testing](https://www.baeldung.com/linux/unit-testing-bash)
- [The Art of Command Line Testing in Bash](https://blog.dcycle.com/blog/2016-11-04-the-art-of-command-line-testing-in-bash/)
- [Bash Testing for Beginners](https://medium.com/@panosa/testing-in-bash-for-beginners-506c3828dae6)