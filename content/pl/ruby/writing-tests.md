---
title:    "Ruby: Pisanie testów"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach tworzenie testów w programowaniu jest niezbędną umiejętnością. Pozwala ono na pewniejsze i bezawaryjne działanie kodu, a także ułatwia późniejszą pracę w przypadku konieczności wprowadzania zmian lub naprawiania błędów. W tym artykule pokażemy, dlaczego warto pisać testy w języku Ruby oraz jak to zrobić w praktyce.

## Jak to zrobić

Pisanie testów w Ruby jest bardzo proste dzięki narzędziu RSpec. Najpierw musimy zainstalować RSpec poprzez wykonanie polecenia `gem install rspec` w terminalu. Następnie, w swoim pliku projektowym, możemy stworzyć plik `spec.rb` i zacząć pisać nasze testy.

```Ruby
# spec.rb
require 'rspec'
require_relative 'my_code' # plik z naszym kodem

describe "MyCode" do
  it "should return the correct output" do
    expect(MyCode.add(2, 3)).to eq(5)
    expect(MyCode.multiply(5, 2)).to eq(10)
  end
end
```

W tym przykładzie tworzymy testy dla dwóch metod `add` i `multiply`. Możemy uruchomić nasze testy wykonując polecenie `rspec spec.rb` w terminalu. Jeśli wszystkie testy przejdą pomyślnie, zobaczymy zielone znaczki oznaczające sukces. W przypadku, gdy któryś test nie przejdzie, zostanie wyświetlony błąd i łatwiej będzie nam go zlokalizować i naprawić.

## Deep Dive

Testy pozwalają na sprawdzenie wszystkich możliwych przypadków działania naszego kodu. Dzięki temu możemy upewnić się, że kod działa nie tylko dla konkretnych wartości, ale także dla różnych typów danych lub w przypadku błędnych danych. Testy mogą również stanowić swojego rodzaju dokumentację naszego kodu, pokazując przykładowe użycie i oczekiwaną wartość.

Kolejną zaletą pisania testów jest możliwość refaktoryzacji kodu. Gdy mamy już działający kod i przeprowadzimy zmiany, możemy uruchomić nasze testy, aby upewnić się, że nie wprowadziliśmy przypadkiem błędów.

Podsumowując, pisanie testów to nie tylko dobry zwyczaj, ale również umiejętność, która pozwala na tworzenie lepszego i bezawaryjnego kodu.

## Zobacz też

- [Oficjalna dokumentacja RSpec](https://rspec.info/)
- [Tutorial o testowaniu w Ruby](https://www.rubyguides.com/2018/07/rspec-tutorial/)
- [Kurs Ruby na Codecademy](https://www.codecademy.com/learn/learn-ruby)