---
title:                "Wysyłanie żądania http"
html_title:           "Ruby: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

Polish readers, welcome to this article about sending HTTP requests in Ruby. If you're wondering why you would ever need to do this, read on to find out!

## Dlaczego

Wysyłanie zapytania HTTP jest niezbędnym elementem w dzisiejszym świecie internetowym. Bez niego nie możemy korzystać z wielu funkcjonalności, takich jak pobieranie danych z serwerów zewnętrznych czy wysyłanie formularzy. Dlatego warto poznać tę umiejętność, aby móc wykorzystać jej potencjał w swoich projektach.

## Jak To Zrobić

Kodując w Ruby, możemy użyć biblioteki standardowej o nazwie Net::HTTP, która umożliwia nam wysyłanie żądań HTTP. Najpierw musimy zaimportować tę bibliotekę poprzez dodanie linijki kodu `require 'net/http'` na początku naszego pliku.

Następnie, aby wysłać zapytanie, musimy utworzyć instancję klasy `Net::HTTP` i podać jej adres URL do żądanego serwera. Na przykład, jeśli chcielibyśmy pobrać stronę główną Google, użylibyśmy kodu `response = Net::HTTP.get(URI('https://www.google.com'))`. Otrzymamy w ten sposób zapisany w zmiennej `response` kod HTML strony głównej Google.

Jeśli chcemy wysłać formularz, musimy utworzyć obiekt `Net::HTTP::Post` i przekazać mu adres URL oraz dane formularza. Następnie używamy metody `request` na instancji klasy `Net::HTTP` i podajemy jako argument utworzony przez nas obiekt `Net::HTTP::Post`. Odpowiedź serwera będziemy otrzymywać analogicznie, zapisując ją w zmiennej i wyświetlając.

# Deep Dive

Istnieje wiele opcji dostępnych w bibliotece Net::HTTP. Możemy na przykład wysyłać zapytania w różnych formatach, takich jak JSON czy XML, używając odpowiednich metod (np. `get(uri)`, `post(uri, data)`, `put(uri, data)`, `delete(uri)`). Możemy również ustawiać nagłówki HTTP, zarówno w żądaniach, jak i odpowiedziach, a także używać protokołów SSL i TLS do bezpiecznego łączenia się z serwerami.

Możliwości są szerokie, więc warto przeczytać dokładną dokumentację biblioteki Net::HTTP oraz przyjrzeć się przykładowym kodom dostępnym w internecie, aby poznać więcej o wykorzystaniu tej funkcjonalności w Ruby.

# Zobacz Również

- Dokumentacja biblioteki Net::HTTP: https://ruby-doc.org/stdlib-2.7.2/libdoc/net/http/rdoc/Net/HTTP.html
- Przykładowy kod wysyłający zapytanie HTTP w Ruby: https://www.rubyguides.com/2018/05/http-requests-ruby/