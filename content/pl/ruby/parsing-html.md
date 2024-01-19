---
title:                "Analiza składniowa HTML"
html_title:           "Gleam: Analiza składniowa HTML"
simple_title:         "Analiza składniowa HTML"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/parsing-html.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Analiza syntaktyczna HTML (parsing) to proces przetwarzania kodu HTML na strukturalne części składowe. Programiści robią to, aby przechwytywać i manipulować danymi zawartymi w kodzie HTML.

## Jak to zrobić:
Ruby daje nam kilka sposobów na analizę kodu HTML. Przyjrzyjmy się sposobowi, w jaki to robimy za pomocą gema Nokogiri.

Zainstaluj gem Nokogiri, jeśli jeszcze go nie masz, wpisując `gem install nokogiri` w terminalu.

Teraz zobaczmy prosty przykład kodu:

```Ruby
require 'nokogiri'
require 'open-uri'

# Otworzy stronę internetową i przeprowadzi parsowanie
doc = Nokogiri::HTML(open('https://www.example.com'))

# Wydrukuj nagłówek strony
puts doc.at_css('title').text
```

W tym przykładzie podajemy URL do metody `open`, a następnie przekazujemy to do Nokogiri do parsowania. Metoda `at_css` pozwala nam wyszukiwać elementy na stronie za pomocą selektorów CSS.

## Na głęboką wodę
Parsowanie HTML ma swoje korzenie w początkach HTML, kiedy programiści musieli ręcznie przetwarzać strony internetowe. Dzisiaj mamy wiele narzędzi, które ułatwiają to zadanie.

Jeśli Nokogiri nie spełnia Twoich wymagań, możesz spróbować alternatyw, takich jak Oga lub mechanize. 

Co do szczegółów implementacji, Nokogiri służy jako interfejs do bibliotek takich jak libxml2 lub xerces, które wykonują właściwą analizę.

## Zobacz także
- Oficjalna strona Nokogiri: https://nokogiri.org/
- Dokumentacja Nokogiri: https://nokogiri.org/rdoc/index.html
- Wiki dla libxml2: http://xmlsoft.org/
- Strona domowa mechanize: https://mechanize.rubyforge.org/