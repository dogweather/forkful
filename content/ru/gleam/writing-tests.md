---
title:                "Написание тестов"
date:                  2024-01-29T00:05:35.442677-07:00
model:                 gpt-4-0125-preview
simple_title:         "Написание тестов"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/gleam/writing-tests.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и зачем?

Написание тестов заключается в создании кода, который проверяет правильность работы другого кода. Программисты делают это, чтобы заранее обнаруживать ошибки, обеспечивать качество и защищать от возможного разрушения функционала при будущих изменениях.

## Как это сделать:

```Gleam
import gleam/should
import my_module

pub fn my_test() {
  // Проверяем, возвращает ли функция ожидаемое значение
  should.equal(my_module.my_function(), "ожидаемый результат")
}

pub fn addition_test() {
  // Тестируем функцию сложения на корректность
  should.equal(my_module.add(1, 2), 3)
}
```

Пример вывода после успешного выполнения набора тестов:

```
Тестируется my_module...
  ✓ my_test пройден
  ✓ addition_test пройден

Все тесты пройдены!
```

## Погружение в глубину

Культура тестирования в Gleam вдохновлена его корнями в Erlang, где ключевым моментом является надежность. Альтернативы, такие как тестирование на основе свойств, также популярны в экосистеме Erlang. С точки зрения реализации, тесты в Gleam - это обычные функции с утверждениями. Они запускаются с помощью исполнителя тестов, а результаты отображаются в понятном для человека формате.

## Смотрите также

- Общее тестирование в Erlang для контекста: [http://erlang.org/doc/apps/common_test/basics_chapter.html](http://erlang.org/doc/apps/common_test/basics_chapter.html)
