---
title:                "Elm: Konwersja ciągu znaków na małe litery"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

### Dlaczego

Programowanie funkcyjne jest jednym z najbardziej popularnych podejść do pisania kodu w dzisiejszych czasach i Elm jest jednym z najciekawszych języków funkcyjnych na rynku. Konwersja ciągu znaków na małe litery jest jednym z podstawowych zadań, które mogą być użyteczne w wielu różnych aplikacjach. W tym artykule przyjrzymy się, jak to zrobić w Elm.

### Jak to zrobić

```Elm
-- Kod przybliżony, jako że zależny od źródła danych!

import Html exposing (text)

inputText : String
inputText = "ELM JEST ZAJEBISTY!"

outputText : String
outputText =
    String.toLower inputText

main =
    text outputText
```

```Elm
-- Output: elm jest zajebisty!
```

W powyższym przykładzie, za pomocą funkcji `toLower` z pakietu `String`, przekształcamy string "ELM JEST ZAJEBISTY!" na "elm jest zajebisty!". Funkcja ta jest bardzo prosta do użycia i nie wymaga dodatkowych argumentów. Można ją zastosować w różnych kontekstach, na przykład do przetwarzania danych wejściowych od użytkownika lub do formatowania tekstu w aplikacji.

### Głębsze zagadnienia

Konwersja ciągu znaków na małe litery w Elm jest możliwa dzięki temu, że jest to język silnie typowany. Komunikacja z użytkownikiem odbywa się przez model, a nie przez bezpośrednie modyfikacje DOM. Dzięki temu, operacje na stringach stają się bezpieczniejsze i łatwiejsze. Co więcej, Elm umożliwia nam wykorzystanie najlepszych praktyk funkcjonalnego programowania, co czyni kod bardziej czytelnym i skalowalnym.

### Zobacz również:

- https://guide.elm-lang.org/effects/random.html
- https://www.elm-tutorial.org/pl/
- http://onlinekeystore.net/blogList?language=Elm&blogid=8901

Dzięki tym linkom będziecie mogli zapoznać się z innymi przydatnymi funkcjami Elm oraz poszerzyć swoją wiedzę na temat tego ciekawego języka. Mam nadzieję, że ten artykuł był dla was pomocny i zachęcił was do dalszej eksploracji Elm. Do zobaczenia!