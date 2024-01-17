---
title:                "Wyszukiwanie i zamiana tekstu"
html_title:           "Elm: Wyszukiwanie i zamiana tekstu"
simple_title:         "Wyszukiwanie i zamiana tekstu"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Zastanawiałeś się kiedyś, jak programiści mogą tak szybko zmieniać setki, a nawet tysiące linii kodu w jednym kroku? To właśnie dzięki technice wyszukiwania i zamiany tekstu. Jest to proces, który pozwala programistom odnaleźć i zmodyfikować określone ciągi znaków w ich kodzie. Jest to niezwykle przydatne narzędzie, które pomaga w szybkim i efektywnym wprowadzaniu zmian w dużych projektach.

## Jak to zrobić:
W Elm istnieje kilka sposobów na przeprowadzenie procesu wyszukiwania i zamiany tekstu. Możesz użyć funkcji `String.replace` aby dokonać zmiany w jednym ciągu znaków lub funkcji `String.replaceList` aby dokonać zmiany w wielu ciągach jednocześnie. Przykładowe użycie tych funkcji wygląda następująco:

```elm
text = "Cześć, witaj w Elm!"
newText = String.replace "witaj" "hej" text

-- newText = "Cześć, hej w Elm!"
```

```elm
phrases = ["Cześć", "witaj"]
newPhrases = String.replaceList phrases "hej" text

-- newPhrases = ["hej", "hej w Elm!"]
```

## Głębsze Zanurzenie:
Technika wyszukiwania i zamiany tekstu jest powszechnie stosowana we wszystkich językach programowania. Pierwsze narzędzia służące do tego celu pojawiły się w latach 70. XX wieku, kiedy to programiści zaczęli szukać sposobów na automatyzację procesu edycji tekstu w programach. Obecnie istnieje wiele alternatywnych narzędzi do wyszukiwania i zamiany tekstu, takich jak edytory tekstu lub narzędzia online. W Elm jednak korzystając z funkcji `String.replace` i `String.replaceList` możemy wygodnie i efektywnie dokonywać zmian w naszym kodzie.

## Zobacz też:
- Dokumentacja funkcji `String.replace` w Elm: https://package.elm-lang.org/packages/elm/core/latest/String#replace
- Dokumentacja funkcji `String.replaceList` w Elm: https://package.elm-lang.org/packages/elm/core/latest/String#replaceList