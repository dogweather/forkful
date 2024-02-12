---
title:                "Написание тестов"
aliases: - /ru/ruby/writing-tests.md
date:                  2024-01-29T00:06:17.811754-07:00
model:                 gpt-4-0125-preview
simple_title:         "Написание тестов"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/ruby/writing-tests.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и зачем?

Написание тестов проверяет, работает ли код как ожидается. Программисты делают это, чтобы заранее обнаружить ошибки, обеспечить надежность и облегчить будущие изменения кода.

## Как это сделать:

В Ruby для тестирования используются Minitest и RSpec — давайте использовать RSpec. Сначала установите его:

```ruby
gem install rspec
```

Создайте тестовый файл, `calculator_spec.rb`:

```ruby
RSpec.describe Calculator do
  describe "#add" do
    it "суммирует два числа" do
      expect(Calculator.new.add(3, 7)).to eql(10)
    end
  end
end
```

Запустите тест с помощью:

```shell
rspec calculator_spec.rb
```

Вывод:

```
F

Ошибки:

  1) Calculator#add суммирует два числа
     Ошибка/Ошибка: expect(Calculator.new.add(3, 7)).to eql(10)
     
     NameError:
       неинициализированная константа Calculator
```

Создайте `calculator.rb`:

```ruby
class Calculator
  def add(a, b)
    a + b
  end
end
```

Снова запустите тесты.

Вывод:

```
.

Завершено за 0.002 секунды (файлы загружены за 0.08 секунд)
1 пример, 0 неудач
```

## Глубокое погружение

Тестирование в Ruby ведет свои корни от Test::Unit, но RSpec, представленный в 2005 году, революционизировал тестирование в Ruby благодаря "разработке, ориентированной на поведение". Альтернативы RSpec включают Minitest и Test::Unit. RSpec сосредоточен на читаемости и бизнес аспектах; Minitest более минималистичен и быстрее. Типично, тесты имитируют использование программного обеспечения, проверяя функции, данные и крайние случаи. Для существующих проектов начните с тестирования наиболее критичных частей.

## Смотрите также

- RSpec GitHub: [github.com/rspec/rspec](https://github.com/rspec/rspec)
- Minitest: [rubygems.org/gems/minitest](https://rubygems.org/gems/minitest)
- "Эффективное тестирование с RSpec 3": Читайте для получения дополнительной информации о принципах и паттернах RSpec.
