---
title:                "Письмо тестів"
date:                  2024-02-03T19:31:00.695821-07:00
model:                 gpt-4-0125-preview
simple_title:         "Письмо тестів"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та Чому?

Написання тестів у оболонці Fish передбачає створення скриптів, які автоматично запускають ваш код для перевірки його поведінки на відповідність очікуваним результатам. Ця практика є важливою, оскільки вона гарантує, що ваші скрипти оболонки працюють належним чином, виявляючи помилки на ранніх етапах та спрощуючи обслуговування.

## Як:

Оболонка Fish не має вбудованого фреймворку для тестування, як деякі інші програмні середовища. Проте, ви можете написати прості тестові скрипти, які використовують утвердження (assertions) для перевірки поведінки ваших функцій. Крім того, ви можете використовувати інструменти сторонніх розробників, як-от `fishtape`, для більш всебічного набору тестів.

### Приклад 1: Простий Тестовий Скрипт

Почнемо з базової функції у Fish, яка розраховує суму двох чисел:

```fish
function add --description 'Додати два числа'
    set -l sum (math $argv[1] + $argv[2])
    echo $sum
end
```

Ви можете написати простий тестовий скрипт для цієї функції таким чином:

```fish
function test_add
    set -l result (add 3 4)
    if test $result -eq 7
        echo "test_add пройшов"
    else
        echo "test_add не пройшов"
    end
end

test_add
```

Запуск цього скрипта виведе:

```
test_add пройшов
```

### Приклад 2: Використання Fishtape

Для більш надійної тестової рішення, ви можете використовувати `fishtape`, генератор тестів TAP для Fish.

Спочатку встановіть `fishtape`, якщо ви ще цього не зробили:

```fish
fisher install jorgebucaran/fishtape
```

Далі створіть тестовий файл для вашої функції `add`, наприклад, `add_test.fish`:

```fish
test "Додавання 3 і 4 дає 7"
    set result (add 3 4)
    echo "$result" | fishtape
end
```

Для запуску тесту використовуйте наступну команду:

```fish
fishtape add_test.fish
```

Приклад виводу може виглядати так:

```
TAP version 13
# Додавання 3 і 4 дає 7
ok 1 - test_add пройшов
```

Це говорить вам про те, що тест пройшов успішно. `fishtape` дозволяє структурувати більш детальні тести та надає інформативний вивід, сприяючи легшому відлагодженню та всебічному покриттю тестами ваших скриптів Fish.