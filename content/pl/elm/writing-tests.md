---
title:                "Pisanie testów."
html_title:           "Elm: Pisanie testów."
simple_title:         "Pisanie testów."
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/writing-tests.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą, prawdopodobnie słyszałeś o pisaniu testów jednostkowych. Możesz się zastanawiać, czy warto poświęcić swój czas na ich tworzenie. Krótko mówiąc - tak, warto! Testy jednostkowe są nieodzownym elementem wytwarzania solidnego i niezawodnego kodu.

## Jak to zrobić

```elm
testAddition : Test
testAddition =
    describe "Addition" (
        test "1 + 1 should equal 2" (
            Expect.equal 2 (1 + 1)
        )
    )
```

Wykorzystując wbudowaną bibliotekę `Test`, możemy łatwo tworzyć testy dla naszego kodu. Przykładowy test pokazuje, jak możemy sprawdzić, czy nasza funkcja dodawania działa poprawnie. Testy pojawią się w okienku wyników jako zielone lub czerwone, informując nas o tym, czy testy zostały wykonane pomyślnie.

```elm
-- 1 
1 + 1
-- 2
2
```

Jeśli wszystko zostało zaimplementowane prawidłowo, testy powinny zwrócić zielony wynik. Jednak jeśli coś zostało zrobione nieprawidłowo, testy poinformują nas w jaki sposób i gdzie wystąpił błąd.

## Głębszy zanurzenie

Pisanie testów jednostkowych może początkowo wydawać się czasochłonne, ale w dłuższej perspektywie może ułatwić nam pracę i oszczędzić dużo czasu. Dzięki testom możemy szybciej wychwycić błędy w naszym kodzie i szybciej je naprawić. Dodatkowo, gdy rozwijamy nasz projekt i wprowadzamy zmiany, testy zapewniają nam pewność, że nic nie zostało przypadkowo uszkodzone.

Przy pisaniu testów warto pamiętać o kilku zasadach:
- Testy powinny być niezależne od siebie - żaden test nie powinien zależeć od wyników innego testu.
- Testy powinny być odzwierciedleniem oczekiwań dla funkcjonalności naszego kodu.
- Dzięki testom możemy zapewnić sobie bezpieczeństwo nawet przy wprowadzaniu większych zmian w naszym kodzie.

Spróbuj przyzwyczaić się do pisania testów jako części swojego procesu programowania, a z czasem zauważysz, że stają się one nieodłącznym elementem wytwarzania wysokiej jakości oprogramowania.

## Zobacz także

- [Oficjalna strona dokumentacji Elm](https://guide.elm-lang.org/)
- [The Coding Train - Elm tutorial dla początkujących](https://www.youtube.com/playlist?list=PLRqwX-V7Uu6ZiZxtDDRCi6uhfTH4FilpH)
- [Cassandra Salisbury - Tworzenie testowalnego kodu w Elm](https://www.youtube.com/watch?v=exjOjW5WgMc)

Teraz, gdy już wiesz jak pisać testy jednostkowe w Elm, czas nauczyć się ich użycia w praktyce. Życzę Ci powodzenia w tworzeniu niezawodnego i solidnego kodu!