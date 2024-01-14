---
title:    "Haskell: Korzystanie z wyrażeń regularnych"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Dlaczego

Regular expressions (wyrażenia regularne) są niezwykle przydatnym narzędziem w każdym języku programowania, a Haskell nie jest tutaj wyjątkiem. Dzięki nim możemy wykonywać zaawansowane manipulacje na ciągach znaków, co jest często niezbędne w pracy programisty. W tym wpisie dowiecie się, dlaczego warto poznać ten podstawowy mechanizm i jak można go wykorzystywać w Haskellu.

## Jak to zrobić

Kodowanie wyrażeń regularnych w Haskellu jest bardzo proste i intuicyjne. Możemy skorzystać z funkcji `=~`, która pozwala na dopasowanie wzorca do podanego ciągu znaków. Na przykład, jeśli chcemy sprawdzić, czy dany tekst zawiera liczbę, możemy napisać:

```
"123" =~ "[0-9]+" :: Bool
```

W powyższym przykładzie używamy wyrażenia regularnego `[0-9]+`, które oznacza dowolną liczbę, składającą się z co najmniej jednej cyfry. Funkcja `=~` zwróci wartość `True`, ponieważ dane wyrażenie znajduje się w podanym tekście.

Możemy również wykorzystać wyrażenia regularne do przechwytywania i manipulowania danymi. Na przykład, jeśli mamy ciąg znaków w formacie "imie_nazwisko", możemy wyodrębnić imię i nazwisko, wykorzystując wyrażenie regularne:

```
"John_Doe" =~ "([A-Za-z]+)_([A-Za-z]+)" :: Maybe (String, String)
```

W tym przypadku używamy nawiasów do przechwycenia wartości imienia i nazwiska z tekstu, a następnie funkcja `=~` zwróci je jako parę wartości w typie `Maybe`.

## Głębsza analiza

W Haskellu używa się składni podobnej do wyrażeń regularnych znanej z innych języków, jednak istnieją również pewne różnice i dodatkowe możliwości. Na przykład, możemy wykorzystać wzorce w wyrażeniach regularnych, dzięki czemu możemy dopasować nie tylko konkretne znaki, ale również różnego rodzaju wzorce, np. cyfry czy litery. Możemy również posługiwać się dodatkowymi funkcjami wyrażeń regularnych, takimi jak `grep`, `replace` czy `split`, które ułatwiają pracę z tekstami.

Jednym z podstawowych mechanizmów wyrażeń regularnych w Haskellu jest operator `=~`, jednak istnieją także inne funkcje, takie jak `=~~` (dla obsługi wieloliniowych tekstów) czy `!~` (dla negatywnego dopasowania), które mogą okazać się przydatne w niektórych przypadkach.

Wyrażenia regularne w Haskellu są również niezwykle wydajne i nie obciążają naszego kodu. Dzięki temu możemy bez obaw używać ich nawet w dużych projektach.

## Zobacz także

* [Oficjalna dokumentacja dla wyrażeń regularnych w bibliotece `Text.RE`](https://hackage.haskell.org/package/regex-doc-0.13.0/docs/Text-RE.html)
* [Poradnik dla początkujących w wyrażeniach regularnych w Haskellu](http://learnyouahaskell.com/regular-expressions)
* [Przykłady użycia wyrażeń regularnych](https://www.stackage.org/haddock/lts-12.26/regex-base-0.93.2/Text-Regex-Base.html#g:10)