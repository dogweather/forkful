---
title:                "Ruby: Skanowanie HTML"
simple_title:         "Skanowanie HTML"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/parsing-html.md"
---

{{< edit_this_page >}}

# Dlaczego warto parsować HTML?

Masz już pewnie dość ręcznego przeglądania i wycinania danych z tysięcy stron internetowych. W końcu nadeszła pora, aby nauczyć się parsować HTML za pomocą Ruby i sprawić, że życie będzie znacznie łatwiejsze. Nie tylko oszczędzisz czas, ale także unikniesz frustracji związanej z ręcznym ergo-wycinaniem kodu HTML.

# Jak to zrobić?

Możesz użyć biblioteki Nokogiri, która umożliwia wygodne parsowanie HTML przy użyciu narzędzi Ruby. Aby zacząć, musisz zainstalować bibliotekę, np. w systemie operacyjnym Mac OS, używając narzędzia Homebrew możesz użyć polecenia `brew install libxml2 libxslt` a następnie zainstalować gem Nokogiri przy użyciu `gem install nokogiri`. Jeśli używasz Windowsa, możesz skorzystać z instrukcji zawartych na stronie biblioteki Nokogiri.

Kiedy już zainstalujesz Nokogiri, możesz rozpocząć parsowanie HTML. Przykładowy kod wyglądałby następująco:

```Ruby
require 'nokogiri'
require 'open-uri'

url = "https://example.com"
page = Nokogiri::HTML(open(url))

page.css("h2").each do |heading|
  puts heading.content
end
```

Ten kod pobiera stronę internetową z adresu "https://example.com", a następnie za pomocą biblioteki Nokogiri znajduje i wyświetla wszystkie nagłówki drugiego poziomu (h2). Kod wewnątrz pętli może zostać zmieniony w zależności od potrzeb, aby wyświetlać i przetwarzać wybrane elementy HTML.

# Głębsze zagadnienia związane z parsowaniem HTML

Parsowanie HTML może być trudne i czasochłonne, ponieważ nie zawsze strony internetowe są poprawnie sformatowane. W zależności od skomplikowania strony, może być konieczne użycie zaawansowanych narzędzi, aby uzyskać pożądane dane. Na szczęście Nokogiri oferuje szeroki wachlarz metod, które pomogą Ci radzić sobie z różnorodnymi przypadkami.

Ponadto, warto pamiętać, że parsowanie HTML jest jedynie jednym ze sposobów na pobieranie i przetwarzanie danych ze stron internetowych. Możliwe jest również wykorzystanie API lub innych narzędzi, jednak parsowanie HTML może być potrzebne, jeśli brak innych dostępnych opcji.

# Zobacz również

- [Dokumentacja biblioteki Nokogiri](https://rubydoc.info/github/sparklemotion/nokogiri)
- [Poradnik w języku polskim o parsowaniu HTML przy użyciu Nokogiri](https://www.profesjonalne-kody.pl/nokogiri-poradnik-parsowanie-html-w-jezyku-ruby/)
- [Przykłady użycia biblioteki Nokogiri](https://nokogiri.org/tutorials/searching_a_xml_html_document.html)