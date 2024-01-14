---
title:    "Haskell: Zmiana na wielkie litery ciągu znaków"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zastanawiałeś się, dlaczego programiści muszą zmieniać wielkość liter w ciągach znaków? Przecież używamy komputerów, które powinny być w stanie wyświetlać wszystkie litery tak, jak chcemy. Jednak w rzeczywistości istnieją różne przypadki, w których zmiana wielkości liter jest niezbędna. W tym artykule dowiesz się, dlaczego i jak możesz to zrobić w programowaniu w języku Haskell.

## Jak to zrobić

Aby zmienić wielkość liter w ciągu znaków w języku Haskell, możesz skorzystać z funkcji "toUpper" lub "toLower" z biblioteki "Data.Char". Przykładowo, aby zmienić wszystkie litery w ciągu na wielkie, możemy użyć funkcji "map":

```Haskell
map toUpper "hello world"
```

Wyjściem tego kodu będzie ciąg "HELLO WORLD". Podobnie, aby zmienić wszystkie litery na małe, możemy użyć funkcji "map" razem z "toLower".

Aby przetworzyć ciąg znaków zawierający cyfry, możesz użyć funkcji "digitToInt" z biblioteki "Data.Char". Przykładowo, jeśli chcesz dodać tylko liczby zawarte w ciągu, możesz zastosować następujący kod:

```Haskell
sum $ map digitToInt "123"
```

Wyjściem tego kodu będzie liczba 6, ponieważ 1 + 2 + 3 = 6.

## Głębsze zagadnienia

Aby lepiej zrozumieć, jak zmiana wielkości liter działa w języku Haskell, musisz wiedzieć kilka rzeczy o funkcjach wyższego rzędu. W języku Haskell funkcje są traktowane tak samo jak inne wartości, dlatego mogą być przekazywane jako argumenty do innych funkcji. Dzięki temu, używając funkcji "map", możemy zastosować inną funkcję do każdego elementu w liście. W naszym przypadku, funkcja "toUpper" jest zastosowana do każdego pojedynczego znaku w ciągu znaków.

Innym ważnym elementem jest stosowanie funkcji w postaci "funkcji składanych" (ang. function composition). Funkcje te pozwalają na wyrażanie złożonych funkcji w sposób bardziej czytelny i zwięzły. W powyższym przykładzie zsumowania cyfr, użycie "function composition" spowodowało, że nasz kod jest wyraźniejszy niż pisząc go w klasyczny sposób.

## Zobacz także

Jeśli jesteś zainteresowany innymi możliwościami przetwarzania ciągów znaków w języku Haskell, może warto zajrzeć do następujących zasobów:

- [Dokumentacja biblioteki "Data.Char"](https://hackage.haskell.org/package/base/docs/Data-Char.html)
- [Tutorial o funkcjach wyższego rzędu w języku Haskell](https://en.wikibooks.org/wiki/Haskell/Higher-order_functions)
- [Tutorial o funkcjach składanych w języku Haskell](https://en.wikibooks.org/wiki/Haskell/Function_composition)

Dziękujemy za lekturę! Mam nadzieję, że ten artykuł pomógł Ci lepiej zrozumieć jak przetwarzać ciągi znaków w języku Haskell. Za pomoc w przygotowaniu tego artykułu dziękujemy ["Write the Docs"](https://write-the-docs.readthedocs.io/en/latest/) oraz ["The Pragmatic Bookshelf"](https://pragprog.com/). Do zobaczenia w kolejnym tekście!