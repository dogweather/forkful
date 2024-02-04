---
title:                "Письмо тестів"
date:                  2024-02-03T19:29:52.244642-07:00
model:                 gpt-4-0125-preview
simple_title:         "Письмо тестів"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/bash/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та Чому?
Написання тестів на Bash передбачає створення скриптових тестових випадків для перевірки функціональності ваших скриптів на Bash. Програмісти проводять тести, щоб переконатися, що їх скрипти працюють як очікується в різних умовах, виявляючи помилки та баги перед розгортанням.

## Як:
Bash не має вбудованої системи тестування, але ви можете написати прості тестові функції. Для більш складного тестування популярні сторонні інструменти, як-от `bats-core`.

### Простий приклад тесту на чистому Bash:
```bash
function test_example_function {
  result=$(your_function 'test_input')
  expected_output="expected_output"
  
  if [[ "$result" == "$expected_output" ]]; then
    echo "Тест пройшов."
    return 0
  else
    echo "Тест не пройшов. Очікувано '$expected_output', отримано '$result'"
    return 1
  fi
}

# Виклик функції тесту
test_example_function
```
Приклад виводу:
```
Тест пройшов.
```

### Використання `bats-core` для тестування:
Спочатку встановіть `bats-core`. Це зазвичай можна зробити через вашого менеджера пакетів або клонуванням його репозиторію.

Потім напишіть свої тести у окремих файл `.bats`.

```bash
# Файл: example_function.bats

#!/usr/bin/env bats

@test "тест прикладної функції" {
  result="$(your_function 'test_input')"
  expected_output="expected_output"
  
  [ "$result" == "$expected_output" ]
}
```
Щоб запустити свої тести, просто виконайте файл `.bats`:
```bash
bats example_function.bats
```
Приклад виводу:
```
 ✓ тест прикладної функції

1 тест, 0 невдач
```

Цей підхід дозволяє легко інтегрувати тестування у ваш робочий процес розробки, забезпечуючи надійність і стабільність ваших скриптів на Bash.
