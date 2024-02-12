---
title:                "Удаление кавычек из строки"
aliases: - /ru/ruby/removing-quotes-from-a-string.md
date:                  2024-01-29T00:01:38.667672-07:00
model:                 gpt-4-0125-preview
simple_title:         "Удаление кавычек из строки"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/ruby/removing-quotes-from-a-string.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и зачем?
Удаление кавычек из строки означает избавление от двойных или одинарных знаков кавычек, окружающих текстовые значения. Программисты часто делают это для очистки пользовательского ввода, чтобы обеспечить согласованность в обработке данных или для подготовки данных к системам, которые могут запутаться из-за этих лишних символов.

## Как:
У Ruby есть несколько хитростей для удаления этих надоедливых кавычек. Вы можете использовать методы `gsub` или `delete` для выполнения этой задачи. Вот некоторый код для размышлений:

```ruby
# Использование gsub для удаления двойных и одинарных кавычек
quoted_string = "\"Say 'hello' to my little friend!\""
unquoted_string = quoted_string.gsub(/'|"/, '')
puts unquoted_string 
# Вывод: Say hello to my little friend!

# Если вы знаете, что будете иметь дело только с одним типом кавычек
single_quoted_string = "'Stay a while and listen!'"
clean_string = single_quoted_string.delete("'")
puts clean_string 
# Вывод: Stay a while and listen!
```

## Подробнее
История кавычек уходит корнями в самые ранние дни программирования, где они часто служили разделителями строк. Сейчас, как и тогда, вам может понадобиться удалить эти символы кавычек, когда они не нужны или когда они могут мешать хранению и манипуляции данными.

Мы говорили о `gsub` и `delete`, но есть и другие методы, как `tr` или `tr_s`, которые дают вам немного больше контроля или могут обрабатывать некоторые другие случаи использования:

```ruby
# tr также может удалять кавычки
double_quoted_string = "\"Do or do not, there is no try.\""
clean_string = double_quoted_string.tr('\"', '')
puts clean_string 
# Вывод: Do or do not, there is no try.
```

Помните, каждый из этих методов имеет свои случаи использования. `gsub` более мощный, когда вы имеете дело со сложными шаблонами или несколькими заменами. `delete` и `tr` прекрасно работают для простого, прямолинейного удаления символов.

## Смотрите также
Для дополнительного чтения и чтобы увидеть эти методы в действии в более крупных кодовых базах, ознакомьтесь с:
- Документацией Ruby для [String#gsub](https://ruby-doc.org/core-3.1.2/String.html#method-i-gsub), [String#delete](https://ruby-doc.org/core-3.1.2/String.html#method-i-delete) и [String#tr](https://ruby-doc.org/core-3.1.2/String.html#method-i-tr).
- У Ruby Monstas есть отличный [набор упражнений на строки](http://ruby-for-beginners.rubymonstas.org/built_in_classes/strings.html), включающий работу с кавычками.
- Обсуждения на Stack Overflow по [манипуляции со строками](https://stackoverflow.com/search?q=ruby+remove+quotes+from+string) предоставляют проблемы и решения из реального мира от других ruby-программистов.
