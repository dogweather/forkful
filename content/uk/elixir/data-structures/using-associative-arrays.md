---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:11:04.005286-07:00
description: "\u0412 Elixir \u0430\u0441\u043E\u0446\u0456\u0430\u0442\u0438\u0432\
  \u043D\u0456 \u043C\u0430\u0441\u0438\u0432\u0438, \u044F\u043A\u0456 \u043D\u0430\
  \u0437\u0438\u0432\u0430\u044E\u0442\u044C\u0441\u044F \u041A\u0430\u0440\u0442\u0438\
  \ (Maps), \u0454 \u043A\u043E\u043B\u0435\u043A\u0446\u0456\u044F\u043C\u0438 \u043F\
  \u0430\u0440 \u043A\u043B\u044E\u0447-\u0437\u043D\u0430\u0447\u0435\u043D\u043D\
  \u044F, \u0434\u0435 \u0443\u043D\u0456\u043A\u0430\u043B\u044C\u043D\u0438\u0439\
  \ \u043A\u043B\u044E\u0447 \u0432\u043A\u0430\u0437\u0443\u0454 \u043D\u0430 \u0437\
  \u043D\u0430\u0447\u0435\u043D\u043D\u044F. \u0412\u043E\u043D\u0438 \u043D\u0430\
  \u0434\u0437\u0432\u0438\u0447\u0430\u0439\u043D\u043E \u0437\u0440\u0443\u0447\u043D\
  \u0456\u2026"
lastmod: '2024-03-13T22:44:48.711203-06:00'
model: gpt-4-0125-preview
summary: "\u0412 Elixir \u0430\u0441\u043E\u0446\u0456\u0430\u0442\u0438\u0432\u043D\
  \u0456 \u043C\u0430\u0441\u0438\u0432\u0438, \u044F\u043A\u0456 \u043D\u0430\u0437\
  \u0438\u0432\u0430\u044E\u0442\u044C\u0441\u044F \u041A\u0430\u0440\u0442\u0438\
  \ (Maps), \u0454 \u043A\u043E\u043B\u0435\u043A\u0446\u0456\u044F\u043C\u0438 \u043F\
  \u0430\u0440 \u043A\u043B\u044E\u0447-\u0437\u043D\u0430\u0447\u0435\u043D\u043D\
  \u044F, \u0434\u0435 \u0443\u043D\u0456\u043A\u0430\u043B\u044C\u043D\u0438\u0439\
  \ \u043A\u043B\u044E\u0447 \u0432\u043A\u0430\u0437\u0443\u0454 \u043D\u0430 \u0437\
  \u043D\u0430\u0447\u0435\u043D\u043D\u044F. \u0412\u043E\u043D\u0438 \u043D\u0430\
  \u0434\u0437\u0432\u0438\u0447\u0430\u0439\u043D\u043E \u0437\u0440\u0443\u0447\u043D\
  \u0456\u2026"
title: "\u0412\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u043D\u043D\u044F \u0430\
  \u0441\u043E\u0446\u0456\u0430\u0442\u0438\u0432\u043D\u0438\u0445 \u043C\u0430\u0441\
  \u0438\u0432\u0456\u0432"
---

{{< edit_this_page >}}

## Що і Чому?

В Elixir асоціативні масиви, які називаються Карти (Maps), є колекціями пар ключ-значення, де унікальний ключ вказує на значення. Вони надзвичайно зручні для зберігання та отримання даних на льоту, роблячи ваш код чистішим і ваше життя легшим.

## Як:

Створення Карти - це просто. Використовуйте синтаксис `%{}`, ось так:

```elixir
my_map = %{"name" => "Олексій", "age" => 32}
IO.inspect(my_map)
```

Доступ до значень відбувається за допомогою ключів:

```elixir
IO.puts my_map["name"]
```
Вивід: `Олексій`

Для додавання або оновлення значень ви можете використати функцію `Map.put/3`:

```elixir
updated_map = Map.put(my_map, "location", "НЮ")
IO.inspect(updated_map)
```
Вивід: `%{"age" => 32, "location" => "НЮ", "name" => "Олексій"}`

Видалення ключів так само просте з `Map.delete/2`:

```elixir
trimmed_map = Map.delete(updated_map, "age")
IO.inspect(trimmed_map)
```
Вивід: `%{"location" => "НЮ", "name" => "Олексій"}`

## Занурення в тему

Карти в Elixir є еволюцією старіших типів зберігання ключ-значення, як-от Хеші в Ruby або Словники у Python. Вони дозволяють здійснювати ефективніші пошуки та вставки, ставши ідеальним вибором для сучасного програмування на Elixir. Варто зазначити, що до Карт Elixir використовував модулі HashDict та Dict, які зараз застарілі.

Втім, для сценаріїв, що потребують упорядкованих даних, ви могли б розглянути ключові списки в Elixir. Це списки кортежів, ефективні для менших колекцій, але не такі зручні для великих наборів даних, як Карти.

Зверніть увагу, що Карти зберігають свої ключі в "плоскій" структурі, що робить прямий доступ до вкладених значень трохи складним. Для глибокого вкладення ви могли б розглянути структурований доступ через функції `get_in`, `put_in`, `update_in`, та `get_and_update_in`, які дозволяють динамічніший підхід до маніпулювання вкладеними даними.

Підсумовуючи, хоча Карти є вашим першочерговим вибором для потреб асоціативних масивів в Elixir, мова пропонує багатий вибір структур даних для кожного сценарію, заохочуючи вас вибирати найкращий інструмент для роботи.
