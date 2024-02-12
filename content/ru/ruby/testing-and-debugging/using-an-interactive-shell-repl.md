---
title:                "Использование интерактивной оболочки (REPL)"
aliases:
- /ru/ruby/using-an-interactive-shell-repl.md
date:                  2024-01-29T00:03:52.207650-07:00
model:                 gpt-4-0125-preview
simple_title:         "Использование интерактивной оболочки (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/ruby/using-an-interactive-shell-repl.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Интерактивная оболочка или REPL (Read-Eval-Print Loop, цикл "чтение-выполнение-вывод") позволяет тестировать код в реальном времени. Программисты используют её для экспериментов, отладки и изучения тонкостей Ruby без создания полноценных скриптов.

## Как это сделать:
REPL для Ruby называется IRB (Interactive Ruby). Попробуйте Ruby прямо из вашего терминала:

```Ruby
irb
2.7.0 :001 > puts "Привет, мир Ruby!"
Привет, мир Ruby!
 => nil
2.7.0 :002 > 5.times { print "Ruby! " }
Ruby! Ruby! Ruby! Ruby! Ruby!  => 5
```

## Подробнее
Введенный в версии Ruby 1.8, IRB является основным инструментом для Ruby-разработчиков. Он вдохновлен интерактивными оболочками Lisp и Python, сочетая в себе экспериментирование с немедленной обратной связью. Альтернативы, такие как Pry, предлагают больше функций, включая подсветку синтаксиса и более надежную среду отладки. Сам по себе IRB прост, но его функциональность может быть расширена с помощью таких гемов, как 'irbtools'. IRB обрабатывает цикл "чтение-выполнение-вывод", читая каждую строку ввода, оценивая её как код Ruby и затем выводя результат, повторяя этот процесс до выхода.

## Смотрите также
- [IRB Ruby](https://ruby-doc.org/stdlib-2.7.0/libdoc/irb/rdoc/IRB.html)
- [Гем irbtools](https://github.com/janlelis/irbtools)
