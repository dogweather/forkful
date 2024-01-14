---
title:                "Ruby: Порівняння двох дат"
programming_language: "Ruby"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

### Чому: 

Порівняння двох дат - це важливий аспект програмування, оскільки дати часто використовуються в різних додатках. Наприклад, ви можете порівняти дату народження користувача з поточною датою, щоб визначити його вік, або порівняти дати для здійснення фільтрації даних за певний період часу.

### Як: 

Для порівняння двох дат використовуються різні методи, такі як `==`, `<`, `>`, `<=`, `>=`. Приклад коду для порівняння двох дат виглядає наступним чином:

```ruby
date1 = Date.new(2020, 10, 15)
date2 = Date.new(2020, 10, 20)

puts date1 == date2 # false
puts date1 < date2 # true
puts date1 > date2 # false
puts date1 <= date2 # true
puts date1 >= date2 # false
```

Вивід цього коду буде наступним:

```no-highlight
false
true
false
true
false
```

### Глибокий аналіз: 

Порівняння дат також може бути здійснене за допомогою методу `between?`, який дозволяє перевірити, чи належить певна дата до заданого періоду. Також, при порівнянні дат, важливо враховувати різні формати та часові зони. Для цього можна використовувати стандартну бібліотеку `time` та методи, такі як `localtime` та `strftime`, для зручного перетворення та виведення дат у бажаному форматі. Наприклад:

```ruby
date1 = Date.new(2020, 10, 15)
date2 = Date.new(2020, 10, 20)

puts date1.between?(date2, Date.today) # true
puts date2.strftime("%B %d, %Y") # October 20, 2020
```

Вивід цього коду буде наступним:

```no-highlight
true
October 20, 2020
```

### Дивись також: 

- [Документація Ruby для порівняння дат](https://ruby-doc.org/stdlib-2.6.3/libdoc/date/rdoc/Date.html)
- [Порівняння дат у Ruby](https://www.rubyguides.com/2019/02/ruby-compare-dates/)
- [Порівняння дат за простими правилами у Ruby](https://www.rubyguides.com/2018/10/ruby-simple-date-comparison/)