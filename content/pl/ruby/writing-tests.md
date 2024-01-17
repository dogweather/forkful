---
title:                "Pisanie testów"
html_title:           "Ruby: Pisanie testów"
simple_title:         "Pisanie testów"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/writing-tests.md"
---

{{< edit_this_page >}}

# Testy w Ruby: Dlaczego i jak programiści je piszą?

## Co & Dlaczego?
Testy to po prostu fragmenty kodu, które służą do potwierdzenia poprawności działania naszego programu. Są one niezwykle ważnym narzędziem w pracy programisty, ponieważ pozwalają nam upewnić się, że nasz kod jest tak naprawdę odporny na błędy i działa zgodnie z oczekiwaniami. Poza tym, testy dają nam pewność, że zmiany w kodzie nie wpłynęły negatywnie na działanie już istniejących funkcji.

## Jak to zrobić:
W Ruby, pisząc testy, będziemy korzystać z narzędzia o nazwie RSpec. Poniżej znajdziesz przykładowy test, który sprawdza, czy funkcja dodająca dwie liczby działa poprawnie:

```Ruby
# definicja testu
RSpec.describe "addition" do
  # przykładowa metoda do testowania
  it "adds two numbers correctly" do
    # warunek, który powinien zwrócić true
    expect(2 + 2).to eq(4)
  end
end
```

## Głębsza analiza:
Pisanie testów ma długą historię w programowaniu, a pierwsze metody testowania pojawiły się już w latach 60. XX wieku. W Ruby jesteśmy też w stanie pisać testy jednostkowe, które sprawdzają pojedyncze elementy kodu, oraz testy integracyjne, które testują działanie kilku elementów na raz. Alternatywą dla RSpec jest framework MiniTest, jednak większość programistów uważa, że RSpec jest prostszy w użyciu i bardziej intuicyjny.

## Zobacz też:
- Oficjalna strona RSpec: https://rspec.info/
- MiniTest: https://rubygems.org/gems/minitest
- Artykuł na temat testowania w Ruby: https://semaphoreci.com/community/tutorials/getting-started-with-rspec