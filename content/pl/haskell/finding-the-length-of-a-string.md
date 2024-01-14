---
title:    "Haskell: Znajdowanie długości ciągu znaków"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Dlaczego

Jednym z najważniejszych zadań w programowaniu jest manipulacja i przetwarzanie danych. Aby to zrobić, często musimy poznać długość określonego ciągu znaków. W tym artykule dowiesz się, jak w języku Haskell znaleźć długość ciągu znaków i jak to może być przydatne w Twoim codziennym programowaniu.

# Jak to zrobić

Jedną z najprostszych metod na znalezienie długości ciągu znaków w Haskellu jest użycie funkcji `length`, która jest już wbudowana w ten język. Wystarczy podać jako argument funkcji ciąg znaków, a następnie otrzymamy wynik w postaci liczby całkowitej.

```Haskell
length "Hello World" -- Output: 11
```

Jeśli chcesz wykorzystać to w praktyce, możesz stworzyć prosty program, który będzie przyjmował tekst od użytkownika i wyświetlał jego długość. Przykładowy kod wyglądałby następująco:

```Haskell
main = do
    putStrLn "Wprowadź dowolny ciąg znaków: "
    userInput <- getLine
    putStrLn ("Długość wprowadzonego ciągu to: " ++ show (length userInput))
```

W powyższym przykładzie korzystamy z funkcji `putStrLn`, aby wyświetlić tekst na ekranie, oraz `getLine`, aby pobrać wprowadzony przez użytkownika ciąg znaków. Następnie, za pomocą funkcji `length` i `show` wyświetlamy długość ciągu wraz z dopiskiem. Przykładowy output dla wywołania programu mogłoby wyglądać tak:

```Haskell
Wprowadź dowolny ciąg znaków:
Hello World
Długość wprowadzonego ciągu to: 11
```

# Deep Dive

Aby lepiej zrozumieć jak działa funkcja `length`, warto prześledzić jej definicję. Jako że jest ona wbudowana w język Haskell, nie musisz samodzielnie jej implementować, ale możesz zajrzeć do dokumentacji, aby zobaczyć w jaki sposób jest zaimplementowana. W skrócie, wygląda to następująco:

```Haskell
length :: [a] -> Int
length [] = 0
length (x:xs) = 1 + length xs
```

Funkcja ta przyjmuje jako argument listę, a zwraca liczbę całkowitą. Jeśli przekażemy pustą listę, czyli `[]`, zostanie zwrócona wartość 0. W przeciwnym przypadku, funkcja ta rekurencyjnie oblicza długość listy poprzez dodawanie 1 do wyniku zwróconego przez wywołanie funkcji `length` z resztą listy `xs`.

Rekurencja jest jedną z podstawowych technik w programowaniu funkcyjnym, a znajomość jej działania może być bardzo pomocna w pisaniu efektywnych i wydajnych programów.

# Zobacz też

- Dokumentacja funkcji `length`: https://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html#v:length
- Wprowadzenie do programowania funkcyjnego w Haskellu (artykuł w języku polskim): http://learnyouahaskell.com/introduction#warto-się-isnąć-z-haskell-a
- Przykładowe programy w Haskellu: https://wiki.haskell.org/Haskell_in_5_steps