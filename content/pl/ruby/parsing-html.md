---
title:                "Przetwarzanie HTML"
date:                  2024-01-20T15:33:27.060255-07:00
simple_title:         "Przetwarzanie HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/parsing-html.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Parsing HTML, czyli analiza kodu HTML, to sposób na wyjęcie specyficznych informacji ze struktury strony internetowej. Programiści robią to, aby automatycznie przetwarzać dane, scrapować web albo weryfikować poprawność kodu HTML.

## Jak to zrobić:
Ruby ma kilka gemów do parsingu HTML, ale jeden z najpopularniejszych to Nokogiri. Oto jak z niego korzystać:

```Ruby
require 'nokogiri'
require 'open-uri'

# Otworzenie strony i stworzenie Nokogiri dokumentu
doc = Nokogiri::HTML(URI.open('https://www.przykladowa-strona.pl'))

# Wyszukiwanie elementów po selektorze CSS
naglowki = doc.css('h1')
naglowki.each do |h1|
  puts h1.content
end
```
To wydrukuje tekst wszystkich elementów `<h1>` na stronie.

## Dogłębniejsze spojrzenie:
Nokogiri to nie tylko gem, to prawdziwa machina do parsowania HTML i XML w Ruby. Powstał w 2008 roku i od tego czasu jest regularnie aktualizowany. Alternatywą jest między innymi Oga, który stawia na minimalizm zależności i szybkość działania.

Parsing HTML to nie tylko wyciąganie tekstu. To także manipulacja strukturą dokumentu i eksport do innego formatu, np. JSON.

Pamiętaj, że parsing dużych dokumentów może być czasochłonny i wymagać dużo pamięci RAM. Dlatego warto rozważyć lazy loading albo przetwarzanie w częściach.

## Zobacz również:
- [Nokogiri dokumentacja](https://nokogiri.org/)
- [Oga gem](https://github.com/YorickPeterse/oga)
- [StackOverflow dyskusje o parsowaniu HTML w Ruby](https://stackoverflow.com/questions/tagged/ruby+nokogiri)
