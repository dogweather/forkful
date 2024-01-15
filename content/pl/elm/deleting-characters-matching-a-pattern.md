---
title:                "Usuwanie znaków odpowiadających wzorcowi."
html_title:           "Elm: Usuwanie znaków odpowiadających wzorcowi."
simple_title:         "Usuwanie znaków odpowiadających wzorcowi."
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek znalazłeś się w sytuacji, gdzie potrzebowałeś usunąć wszystkie znaki pasujące do pewnego wzoru z tekstu? Może chciałeś usunąć wszystkie liczby lub  znaki interpunkcyjne? W tym artykule dowiesz się, dlaczego i jak możesz dokonać tego w Elm.

## Jak to zrobić

Najpierw musimy zdefiniować nasz tekst wejściowy w Elm, w formie listy znaków:

```Elm
inputText = "To jest przykładowy tekst, w którym wanto usunąć wszystkie spacje."
inputList = String.toList inputText
```

Następnie możemy użyć wbudowanej funkcji `List.filter` do przefiltrowania listy znaków i usunięcia niepotrzebnych znaków. W tym przypadku chcemy usunąć spacje, więc musimy utworzyć funkcję, która zwraca `False` dla spacji:

```Elm
removeSpaces : Char -> Bool
removeSpaces char =
    if char == ' ' then
        False
    else
        True
        
filteredInput = List.filter removeSpaces inputList
```

Ostatnim krokiem jest zmiana listy znaków z powrotem na tekst:

```Elm
outputText = String.fromList filteredInput
```

I to wszystko! Nasz outputText będzie teraz zawierał tekst bez spacji.

## Deep Dive

W powyższym przykładzie skupiliśmy się tylko na usuwaniu spacji, ale funkcja `removeSpaces` może być zmodyfikowana, aby usunąć różne znaki pasujące do różnych wzorów. Na przykład, możemy zmienić funkcję tak, aby usuwała wszystkie liczby:

```Elm
removeNumbers : Char -> Bool
removeNumbers char =
    if char >= '0' && char <= '9' then
        False
    else
        True
        
filteredInput = List.filter removeNumbers inputList
```

Możemy również użyć wyrażeń regularnych, korzystając z paczki `elm/regex` i funkcji `Regex.replace` do dokładnego określenia wzoru, który chcemy usunąć.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o funkcjach wbudowanych w Elm, można zapoznać się z dokumentacją na oficjalnej stronie: https://elm-lang.org/docs. Jeśli interesuje Cię również korzystanie z wyrażeń regularnych w Elm, zobacz tę paczkę: https://package.elm-lang.org/packages/elm/regex/latest/.