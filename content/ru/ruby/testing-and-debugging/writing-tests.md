---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:06:17.811754-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412 Ruby \u0434\u043B\u044F \u0442\u0435\u0441\u0442\u0438\u0440\
  \u043E\u0432\u0430\u043D\u0438\u044F \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\
  \u044E\u0442\u0441\u044F Minitest \u0438 RSpec \u2014 \u0434\u0430\u0432\u0430\u0439\
  \u0442\u0435 \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u0442\u044C\
  \ RSpec. \u0421\u043D\u0430\u0447\u0430\u043B\u0430 \u0443\u0441\u0442\u0430\u043D\
  \u043E\u0432\u0438\u0442\u0435 \u0435\u0433\u043E."
lastmod: '2024-03-13T22:44:46.004009-06:00'
model: gpt-4-0125-preview
summary: "\u0412 Ruby \u0434\u043B\u044F \u0442\u0435\u0441\u0442\u0438\u0440\u043E\
  \u0432\u0430\u043D\u0438\u044F \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u044E\
  \u0442\u0441\u044F Minitest \u0438 RSpec \u2014 \u0434\u0430\u0432\u0430\u0439\u0442\
  \u0435 \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u0442\u044C\
  \ RSpec."
title: "\u041D\u0430\u043F\u0438\u0441\u0430\u043D\u0438\u0435 \u0442\u0435\u0441\u0442\
  \u043E\u0432"
weight: 36
---

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
