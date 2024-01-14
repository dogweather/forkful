---
title:    "Ruby: Читання аргументів командного рядка"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Чому

Командний рядок - це потужний інструмент у програмуванні, який дозволяє збільшити ефективність та швидкість роботи програми. Читання аргументів командного рядка є важливим навичкою для будь-якого програміста, який хоче створювати більш гнучкі та корисні програми.

## Як

Щоб читати аргументи командного рядка в Ruby, використовуйте змінну `ARGV`, яка містить масив переданих параметрів. Наприклад, якщо ви запускаєте програму `ruby hello.rb John 26`, змінна `ARGV` буде містити два елементи: `["John", "26"]`. Можна отримувати доступ до цих елементів за допомогою індексів, наприклад `ARGV[0]` поверне `"John"`, а `ARGV[1]` - `"26"`. Давайте подивимося на приклад коду:

```Ruby
# hello.rb
puts "Привіт #{ARGV[0]}! Тобі #{ARGV[1]} років."
```

```Ruby
$ ruby hello.rb John 26
Привіт John! Тобі 26 років.
```

Як бачите, ми використовуємо змінні `ARGV` для виведення введених імен та віку.

## Глибоке занурення

Крім доступу до аргументів командного рядка, ми також можемо передавати їм параметри. Наприклад, при виклику програми `ruby hello.rb John 26 -g` змінна `ARGV` буде містити `["John", "26", "-g"]`. Ми можемо перевірити наявність параметра `-g`, перевіривши, чи є він у масиві `ARGV`, а потім виконати відповідні дії. Наприклад:

```Ruby
# hello.rb
name = ARGV[0]
age = ARGV[1].to_i

if ARGV.include?("-g")
  puts "Привіт, господарю #{name}. Тобі #{age} років."
else
  puts "Привіт #{name}! Тобі #{age} років."
end
```

```Ruby
$ ruby hello.rb John 26 -g
Привіт, господарю John. Тобі 26 років.
```

За допомогою аргументів командного рядка ми можемо зробити наші програми більш гнучкими та придатними до різних сценаріїв використання.

## Дивіться також

- [Документація Ruby про командний рядок](https://ruby-doc.org/core-2.7.0/ARGF.html)
- [Основи програмування на Ruby](https://rubygarage.org/blog/basics-of-ruby-programming-language)
- [Робота з файлами в Ruby](https://www.rubyguides.com/2015/05/working-with-files-ruby/)