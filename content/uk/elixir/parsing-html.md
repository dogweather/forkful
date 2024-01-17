---
title:                "Розбір html."
html_title:           "Elixir: Розбір html."
simple_title:         "Розбір html."
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/parsing-html.md"
---

{{< edit_this_page >}}

### Що і чому?

Парсинг HTML це процес, в якому програмний код аналізує HTML-код і витягує з нього корисну інформацію. Це важливо для програмістів, тому що це дозволяє їм отримати певні дані з веб-сторінок, які можна використовувати в своїх програмах.

### Як це зробити:

Приклад коду, що використовує Elixir для парсингу HTML:

```Elixir
defmodule Parser do
  require HTTPoison
  
  def get_article(url) do
    response = HTTPoison.get(url)            # використовуємо бібліотеку HTTPoison для отримання веб-сторінки
    regex = ~r/<h1>(.*?)<\/h1>/              # регулярний вираз для пошуку заголовку статті
    article = Regex.run(regex, response.body) # витягуємо заголовок з HTML-коду
    IO.puts hd(article)                      # виводимо перший елемент зі списку - заголовок статті
  end
end

Parser.get_article("https://example.com/article") # викликаємо функцію та передаємо URL сторінки
```

Результат: "Це приклад заголовку статті".

### Глибоке погруження:

Парсинг HTML використовується в програмуванні вже давно і має багато альтернативних інструментів, таких як Beautiful Soup для Python та Nokogiri для Ruby. Екосистема Elixir також має багато бібліотек для роботи з HTML, які можна використовувати замість HTTPoison, наприклад Floki або Parsley.

Якість парсингу залежить від якості HTML, інколи може бути складно витягти потрібну інформацію зі складними розмітками або неправильно сформованим HTML. Також важливо враховувати, що структура HTML може змінюватися, тому потрібно бути готовим до ситуацій, коли парсинг перестане працювати і будуть потрібні зміни у коді.

### Дивіться також:

Документація Elixir про парсинг HTML: https://hexdocs.pm/html/Html.html

Бібліотека Floki: https://github.com/philss/floki

Бібліотека Parsley: https://github.com/Frost/parsley