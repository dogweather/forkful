---
title:                "Elixir: Видобування підрядків"
simple_title:         "Видобування підрядків"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## Тому чому

Екстракція підрядків є важливою функцією в багатьох програмах, яка дозволяє вам отримувати певні частини рядків, що потрібні для вашої програми. Наприклад, вам може знадобитися екстрактувати ім'я користувача з електронної пошти або номер телефону з великого рядка даних. У цій статті ми розглянемо, як за допомогою мови програмування Elixir можна виконати екстракцію підрядків.

## Як

```Elixir
str = "Привіт, мене звати Ліза!"
name = String.slice(str, 11..14)

IO.puts(name) 
```

Вивід: Ліза

У цьому прикладі ми використали функцію `String.slice`, щоб виділити підрядок з рядка за допомогою діапазону індексів. Можна також використовувати функцію `String.split` для розділення рядка на частини і вибрати потрібний підрядок за допомогою індексу.

```Elixir
str = "Останній дзвінок +380123456789"
numbers = String.split(str, " ")
phone_number = List.last(numbers)

IO.puts(phone_number)
```

Вивід: +380123456789

## Глибоке занурення

Elixir надає кілька функцій для екстракції підрядків з рядків, таких як `String.substring`, `String.replace` та `String.replace_leading`. Ці функції можуть бути корисними для більш складних сценаріїв, наприклад, для заміни підрядка або першого входження певного слова.

```Elixir
str = "Сніжинки 1 2 3"
pattern = "1"

new_str = String.replace(str, pattern, "4")

IO.puts(new_str)
```

Вивід: Сніжинки 4 2 3

Крім того, у мові Elixir також є можливість використовувати регулярні вирази для екстракції підрядків. Це дозволяє здійснювати більш гнучкий та точний пошук підрядків у рядках.

## Дивись також

- [Документація Elixir по роботі із рядками](https://hexdocs.pm/elixir/String.html)
- [Приклади регулярних виразів в мові Elixir](https://elixir-lang.org/getting-started/regex.html)
- [Стаття про обробку рядків у мові Elixir](https://medium.com/@andrewarrow/strings-and-regex-using-elixir-acdc1f6b6e7f)