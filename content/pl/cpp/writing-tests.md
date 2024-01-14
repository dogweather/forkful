---
title:                "C++: Pisanie testów"
simple_title:         "Pisanie testów"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/writing-tests.md"
---

{{< edit_this_page >}}

# Dlaczego warto pisać testy w programowaniu?

Testy są nieodłączną częścią procesu tworzenia oprogramowania. Pomagają nam weryfikować poprawność naszego kodu i zapobiegać błędom, a także ułatwiają późniejsze prace związane z modyfikacją lub rozszerzaniem aplikacji. W tym artykule dowiesz się, dlaczego warto pisać testy w C++ i jak to zrobić.

## Jak to zrobić?

Jedną z metod tworzenia testów w C++ jest wykorzystanie biblioteki do testowania np. Google Test lub Boost.Test. Poniżej przedstawione są przykładowe funkcje testujące z wykorzystaniem biblioteki Google Test.

```C++
TEST(DodawanieTest, SprawdzPoprawnoscWyniku) {
    ASSERT_EQ(4, 2+2); // asercja sprawdzająca równość oczekiwanego wyniku z rzeczywistym
}

TEST(SortowanieTest, SprawdzSortowanieRosnace) {
    int tablica[] = {4, 2, 6, 1, 8, 5};
    std::sort(tablica, tablica+6); // sortowanie tablicy rosnąco
    ASSERT_TRUE(std::is_sorted(tablica, tablica+6)); // asercja sprawdzająca, czy tablica jest posortowana
}
```

By uruchomić testy, należy utworzyć nowy projekt z wykorzystaniem wybranej biblioteki, a następnie wkleić powyższe funkcje do odpowiedniego pliku i uruchomić.

## Głębokie zanurzenie

Warto pamiętać, że testy powinny być równie starannie pisane, co kod produkcyjny. Muszą być czytelne, przejrzyste i dobrze nazwane. Należy również zadbać o pokrycie testami różnych przypadków, aby uniknąć błędów w różnych scenariuszach działania aplikacji. Ważne jest również regularne wykonywanie testów w trakcie rozwoju projektu, aby szybko wykrywać ewentualne błędy.

## Zobacz także

- [Przykład wykorzystania Google Test](https://github.com/google/googletest/blob/master/googletest/samples/sample1_unittest.cc)
- [Dokumentacja biblioteki Boost.Test](https://www.boost.org/doc/libs/1_69_0/libs/test/doc/html/boost_test/getting_started/for_more_experienced.html)
- [24 grudnia – testowanie w programowaniu](https://agatagorska.com/advent-of-code-24-grudnia-testowanie-w-programowaniu/)