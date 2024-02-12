---
title:                "Округление чисел"
date:                  2024-01-29T00:01:49.534806-07:00
model:                 gpt-4-0125-preview
simple_title:         "Округление чисел"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/elixir/rounding-numbers.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Округление чисел означает их корректировку до ближайшего значения для упрощения или для достижения определенной точности. Это полезно для улучшения читаемости, сокращения объема хранения или удовлетворения специфических потребностей отрасли, например, при расчетах с деньгами, когда вы хотите округлить до ближайшего цента.

## Как это сделать:
В Elixir вы можете использовать `Float.round/2` для округления числа с плавающей точкой. Вы можете указать количество десятичных знаков, которые хотите сохранить. Вот как это работает:

```elixir
# Округлить число до нуля десятичных знаков
Float.round(3.14159) # => 3.0

# Округлить число до 2 десятичных знаков
Float.round(3.14159, 2) # => 3.14

# Округлить число с отрицательной точностью до ближайшего 10
Float.round(123.456, -1) # => 120.0
```

## Подробнее
Округление чисел — классическая проблема в информатике, настолько, что выбор стратегии округления может повлиять на финансовые системы, научные расчеты и многое другое. `Float.round/2` в Elixir по умолчанию использует округление "к большему полу", которое напоминает традиционное округление, изучаемое в школе.

Если вам нужны другие типы округления, Elixir позволяет создать собственные. Рассмотрим, например, округление "вниз" (всегда к меньшему) или округление "вверх" (всегда к большему). Вы бы использовали `Float.floor/1` или `Float.ceil/1` соответственно.

```elixir
# Округление вниз
Float.floor(3.999) # => 3.0

# Округление вверх
Float.ceil(3.001) # => 4.0
```

Эти альтернативы помогают адаптировать округление к точным потребностям вашего приложения, будь то финансовые расчеты, рендеринг графики или приближение данных.

## См. также
Для дополнительной информации о функциях округления в Elixir и числах с плавающей точкой:

- Официальная документация Elixir по `Float`: https://hexdocs.pm/elixir/Float.html
- Стандарт IEEE для арифметики с плавающей точкой (IEEE 754): https://ieeexplore.ieee.org/document/4610935