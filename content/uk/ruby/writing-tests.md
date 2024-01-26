---
title:                "Написання тестів"
html_title:           "Arduino: Написання тестів"
simple_title:         "Написання тестів"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/writing-tests.md"
---

{{< edit_this_page >}}

## Що це таке & Навіщо?
Тестування коду - це перевірка, щоб програма працювала правильно. Програмісти пишуть тести для виявлення помилок, гарантування якості та уникнення проблем у майбутньому.

## Як це зробити:
```Ruby
# Приклад простого тесту використовуючи RSpec

# Встановити RSpec (у командному рядку):
gem install rspec

# Spec файл
# game_spec.rb
require_relative 'game'

describe Game do
  it "scores a gutter game" do
    game = Game.new
    20.times { game.roll(0) }
    expect(game.score).to eq(0)
  end
end

# Код програми
# game.rb
class Game
  def initialize
    @score = 0
  end

  def roll(pins)
    @score += pins
  end

  def score
    @score
  end
end

# Виконання тесту
# В командному рядку:
rspec game_spec.rb

# Вивід
# .
#
# Finished in 0.00276 seconds (files took 0.114 seconds to load)
# 1 example, 0 failures
```

## Поглиблений аналіз:
Тестування коду - не новий тренд, воно стало частиною програмування з часів ранніх мов, таких як SUnit для Smalltalk. Альтернативами RSpec в Ruby можуть бути MiniTest, Test::Unit, або Cucumber для BDD. Деталі реалізації включають написання тестових сценаріїв, що імітують потенційні паттерни користувача та перевірка відповідності очікувань.

## Дивіться також:
- [RubyTapas: Free Screencasts about Ruby Testing](https://www.rubytapas.com/)
- [RSpec GitHub repository](https://github.com/rspec/rspec)
