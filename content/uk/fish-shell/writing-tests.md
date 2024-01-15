---
title:                "Написання тестів"
html_title:           "Fish Shell: Написання тестів"
simple_title:         "Написання тестів"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/writing-tests.md"
---

{{< edit_this_page >}}

## Чому 

Тести допомагають убезпечувати ваш код від помилок, зберігають його працездатним і знижують ризик збою програми.

## Як 

Існує багато різних типів тестів, але ми будемо фокусуватися на модульних тестах. Це тести, що перевіряють окремі функції або блоки коду. Давайте подивимося на приклад тесту для функції, яка перевіряє, чи є деяка змінна позитивним числом:

```
Fish Shell test -s source_code scope_name
function is_positive(number)
    if test $number -gt 0
        echo "This number is positive"
    else
        echo "This number is negative"
    end
end

source_code='function is_positive(number)
    if test $number -gt 0
        echo "This number is positive"
    else
        echo "This number is negative"
    end
end'

scope_name="Positive Number Test"

result="$(echo $source_code | fish -c "$scope_name")"
if test "$result" = "This number is positive"
    echo "Test passed"
else
    echo "Test failed"
end
```

Як бачите, ми створили тест, який перевіряє наш код на предмет правильності виводу для позитивного числа. Запускаємо його командою `fish -c` за допомогою параметру `-s` для передачі нашого коду і параметру `-s` для назви тесту.

## Глибоке погруження 

При написанні тестів, важливо враховувати як можливо більшу кількість сценаріїв для перевірки вашого коду. Також слід пам'ятати, що тести повинні бути коректно іменовані і організовані для зручності використання.

## Дивись також 

- The Fish Shell [official documentation](https://fishshell.com/docs/current/).
- A [tutorial](https://hackernoon.com/learning-fish-the-famous-command-line-shell-newbies-will-love-e146eaaf8262) on how to use Fish Shell.