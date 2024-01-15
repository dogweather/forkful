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

## Dlaczego

Czy kiedykolwiek zastanawiałeś się dlaczego programiści spędzają tyle czasu na pisaniu testów? Przecież to tylko kolejny etap w procesie tworzenia oprogramowania. Jednak naprawdę warto poświęcić ten dodatkowy wysiłek. Pisząc testy, możemy mieć pewność, że nasz kod jest funkcjonalny i nie dopuszcza do błędów, co przekłada się na wyższą jakość i niezawodność finalnego produktu.

## Jak to zrobić

Aby przetestować swój kod w języku Ruby, możemy wykorzystać framework testowy o nazwie "RSpec". W pierwszym kroku zainstalujmy go przy użyciu polecania `gem install rspec`. Następnie, stwórzmy nowy projekt Ruby i dodajmy do niego folder `spec`. W tym folderze będziemy przechowywać nasze testy.

Teraz, gdy mamy już odpowiednie narzędzia, możemy przystąpić do pisania samych testów. Poniżej przedstawiam przykład testu jednostkowego wraz z wyjaśnieniami:

```Ruby
# Ten test sprawdza, czy suma dwóch liczb jest poprawna.
# Zaczynamy od podłączenia biblioteki "RSpec".
require 'rspec'

# Następnie definiujemy nazwę testu.
RSpec.describe 'Dodawanie' do
  # Wewnątrz bloku describe możemy zagnieżdżać kolejne bloki.
  # W tym przypadku tworzymy blok dla metody "sum".
  describe '#sum' do
    # Tego typu blok używamy, gdy testujemy konkretną funkcję.
    context 'gdy oba argumenty są liczbami' do
      # Podajemy nazwę naszego testu.
      it 'zwraca poprawną sumę' do
        # Tworzymy obiekt, na którym będziemy testować metodę "sum".
        calculator = Calculator.new
        # Wywołujemy metodę "sum" z dwoma argumentami.
        result = calculator.sum(2, 3)
        # W tym miejscu definiujemy oczekiwany wynik.
        expected_result = 5
        # Porównujemy wyniki z użyciem metody "expect".
        expect(result).to eq(expected_result)
      end
    end
  end
end
```
Aby uruchomić nasz test, musimy przejść do folderu z projektem i wpisać w konsoli komendę `rspec spec`, która uruchomi wszystkie testy w folderze `spec`.

## Deep Dive

Pisząc testy, musimy pamiętać o kilku rzeczach:

1. Każdy test powinien być opisowy i czytelny. Dzięki temu łatwiej będzie nam zlokalizować błąd, gdy test nie przejdzie.

2. Musimy sprawdzać warunki brzegowe. Często pomijamy takie przypadki, a to właśnie one mogą powodować błędy w naszym kodzie.

3. Pamiętajmy o testach jednostkowych oraz testach integracyjnych. Te pierwsze odpowiadają za testowanie pojedynczych funkcji, natomiast te drugie za poprawne działanie całego systemu.

4. Przeprowadzajmy testy często, aby na bieżąco weryfikować nasz kod. Dzięki temu unikniemy problemów w przyszłości.

## Zobacz również

- [RubySpec](https://www.rubydoc.info/github/rubyspec/rubyspec)
- [Tutorial RSpec](https://www.youtube.com/watch?v=JhR9IbX0DR8)
- [The RSpec Book](https://pragprog.com/book/achbd/the-rspec-book)