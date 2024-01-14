---
title:    "Haskell: Wyszukiwanie i zamiana tekstu"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

Warto zainteresować się wyszukiwaniem i zamianą tekstu w Haskellu, ponieważ jest to bardzo przydatne narzędzie w tworzeniu aplikacji. Dzięki temu możemy szybko i łatwo zmieniać tekst w naszym kodzie, co przyspiesza naszą pracę i umożliwia wprowadzanie zmian w dużej ilości plików jednocześnie.

## Jak To Zrobić

Aby rozpocząć wyszukiwanie i zamianę tekstu w Haskellu, musimy najpierw zaimportować odpowiedni moduł:

```Haskell
import Text.Regex
```

Następnie możemy użyć funkcji `subRegex` w celu zamiany tekstu. Przykładowy kod może wyglądać następująco:

```Haskell
result = subRegex (mkRegex "stary") "nowy" "To jest stary przykład tekstu."
```

Dzięki temu kodowi wszystkie wystąpienia słowa "stary" zostaną zamienione na słowo "nowy". Wynik wywołania tej funkcji będzie następujący:

```Haskell
"To jest nowy przykład tekstu."
```

Możemy również wykorzystać wyrażenia regularne do bardziej precyzyjnego wyszukiwania i zamiany tekstu. Na przykład, jeśli chcemy zamienić wszystkie cyfry w tekście na ich kwadraty, możemy użyć następującego kodu:

```Haskell
result = subRegex (mkRegex "\\d+") (\m -> show (read (matchStr m) :: Int) ^ 2) "10, 20, 30"
```

Wynik tego kodu będzie następujący:

```Haskell
"100, 400, 900"
```

## Deep Dive

Funkcja `subRegex` przyjmuje trzy argumenty: wyrażenie regularne, wyrażenie funkcyjne do zamiany i tekst, w którym chcemy dokonać zamiany. Pierwszy argument określa wzorzec do wyszukania, drugi określa, co zrobić z dopasowanymi fragmentami, a trzeci to tekst, w którym będziemy szukać.

Funkcja `mkRegex` służy do kompilowania ciągu znaków do wyrażenia regularnego, które następnie możemy przekazać jako argument do funkcji `subRegex`.

W przykładzie wykorzystaliśmy `\d+`, co oznacza, że szukamy dowolnej sekwencji cyfr w tekście. Zamiast tego, możemy wykorzystać dowolne wyrażenie regularne, aby uzyskać dokładniejsze dopasowanie, na przykład `[A-Za-z]+` dla tekstu zawierającego tylko litery.

Funkcja `subRegex` jest również bardzo wydajna, ponieważ używa techniki tzw. drzewa Trie do wyszukiwania. Jest to specjalna struktura danych, która pozwala na szybkie dopasowywanie wyrażeń w tekście.

## Zobacz także

- [Dokumentacja modułu Text.Regex w Haskellu](https://hackage.haskell.org/package/regex-tdfa/docs/Text-Regex.html)
- [Przewodnik po wyrażeniach regularnych w Haskellu](https://wiki.haskell.org/Regular_expressions)
- [Przykłady wykorzystania funkcji `subRegex`](https://stackoverflow.com/questions/23173667/how-to-replace-a-substring-in-haskell)