---
title:                "Аналіз дати з рядка"
html_title:           "C++: Аналіз дати з рядка"
simple_title:         "Аналіз дати з рядка"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Що і чому?

Парсинг дати з рядка - це процес обробки рядка, що містить дату, і перетворення цієї дати у більш управліний формат (наприклад, у об'єкт `Date` в Ruby). Програмісти роблять це, єйкщо дати зберігаються у рядках, проте потребують обробки як дати.

## Як робити:

```ruby
require 'date'

string_date = "2023-12-25"
parsed_date = Date.parse(string_date)

puts parsed_date
# Output: 2023-12-25
```

Вищенаведений код Ruby бере рядок "2023-12-25" і перетворює його на об'єкт `Date` за допомогою методу `parse`.

## Пірнання в глибину

**Історичний контекст:** парсинг дати з рядка завжди був важливим для програмістів. Раніше, дані були майже завжди збережені як рядки, а програмісти перетворювали ці рядки на дати, коли це було необхідно.

**Альтернативи:** Існує багато способів робити це в Ruby. На відміну від методу `parse`, ви також можете використовувати `strptime` для парсингу рядка з датою:

```ruby
require 'date'

string_date = "25-12-2023"
parsed_date = Date.strptime(string_date, '%d-%m-%Y')

puts parsed_date
# Output: 2023-12-25
```
Метод `strptime` дозволяє визначити власний формат дати.

**Реалізація:** перетворення рядка на дату здійснюється шляхом інтерпретації даних рядка, згідно з відомим форматом дати.

## Дивись також:

- Ruby Документація по 'date': https://ruby-doc.org/stdlib-2.5.1/libdoc/date/rdoc/Date.html
- StackOverflow: Обговорення методів 'parse' і 'strptime': https://stackoverflow.com/questions/4491329/when-should-i-use-strptime-date.