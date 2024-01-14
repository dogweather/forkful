---
title:    "Ruby: Porównywanie dwóch dat"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

# Dlaczego

Porównywanie dwóch dat może być ważnym aspektem w pisaniu kodu, ponieważ pozwala nam na sprawdzenie, czy jedna data jest wcześniejsza, późniejsza lub równa drugiej. Jest to szczególnie przydatne w przypadku tworzenia aplikacji, które muszą brać pod uwagę daty, takie jak rezerwacja biletów lotniczych lub organizowanie wydarzeń.

# Jak to zrobić

Aby porównać dwie daty w Ruby, możemy użyć metody `==` lub `eql?`. Metoda `==` sprawdza, czy dwie daty są dokładnie takie same, podczas gdy `eql?` sprawdza również czy są to dokładnie te same obiekty. Przyjrzyjmy się przykładowemu kodowi:

```Ruby
date1 = Date.new(2020, 10, 18)
date2 = Date.new(2020, 10, 18)

puts date1 == date2
# Output: true
puts date1.eql?(date2)
# Output: true
```

W tym przykładzie, `date1` i `date2` są dokładnie takie same, więc zarówno `==` jak i `eql?` zwracają wartość `true`.

Możemy również użyć metod `>` i `<` do porównywania dwóch dat. Jest to przydatne, gdy chcemy sprawdzić, która data jest wcześniejsza lub późniejsza. Przykładowo:

```Ruby
date1 = Date.new(2020, 10, 18)
date2 = Date.new(2020, 10, 21)

puts date1 > date2
# Output: false
puts date1 < date2
# Output: true
```

W tym przykładzie, `date1` jest wcześniejszą datą niż `date2`, więc `date1 < date2` zwraca wartość `true`, a `date1 > date2` zwraca wartość `false`.

# Głębsza analiza

Ruby ma również wiele innych metod, które pozwalają nam na porównywanie dat. Jedną z nich jest metoda `between?`, która sprawdza, czy jedna data znajduje się pomiędzy innymi dwiema datami. Przykładowo:

```Ruby
date1 = Date.new(2020, 10, 18)
start_date = Date.new(2020, 10, 15)
end_date = Date.new(2020, 10, 20)

puts date1.between?(start_date, end_date)
# Output: true
```

W tym przypadku, `date1` jest pomiędzy `start_date` a `end_date`, więc metoda `between?` zwraca wartość `true`.

Możemy również użyć metody `strftime` do porównywania dat, która pozwala nam na określenie formatu w jakim ma zostać zwrócona data. Przykładowo:

```Ruby
date1 = Date.new(2020, 10, 18)
date2 = Date.today
puts date1.strftime("%d/%m/%Y") == date2.strftime("%d/%m/%Y")
# Output: true
```

W tym przykładzie, użyliśmy metody `strftime` do określenia formatu daty jako "dzień/miesiąc/rok". Następnie porównaliśmy `date1` i `date2`, które są w tym samym formacie, więc metoda `strftime` zwraca wartość `true`.

# Zobacz także

Jeśli chcesz dowiedzieć się więcej o porównywaniu dat w Ruby, zapoznaj się z poniższymi linkami:

- [Ruby Date Class - Dokumentacja](https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/Date.html)
- [Porównywanie dat w Ruby - Poradnik](https://www.geeksforgeeks.org/ruby-date-comparing-dates/)
- [Porównywanie dat w Ruby - Tutorial](https://www.tutorialspoint.com/ruby/ruby_date_comparison.htm)