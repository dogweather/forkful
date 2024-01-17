---
title:                "Używanie wyrażeń regularnych"
html_title:           "Elm: Używanie wyrażeń regularnych"
simple_title:         "Używanie wyrażeń regularnych"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?

Wykorzystywanie regularnych wyrażeń to częsty element w pracy programistów. Są to wzorce, które pomagają w znajdowaniu określonych ciągów znaków w tekście. Programiści stosują je, aby szybko i skutecznie przetestować oraz manipulować danymi. Dzięki nim można bez problemu wyodrębnić numery telefonów, adresy email czy zakodowane informacje.

## Jak to zrobić?

Aby użyć regularnych wyrażeń w Elm, należy pobrać moduł "Regex" z biblioteki standardowej. Następnie wykorzystuje się funkcje takie jak "Regex.match" lub "Regex.contains", aby przetestować czy dany wzorzec znajduje się w tekście. Zobacz poniższy przykład:

```Elm
import Regex exposing (match)

-- Sprawdzamy, czy w tekście znajduje się wyraz "wesoły"
match "Cześć wesoły wesoły!" (Regex.regex "wesoły") 
-- Zwraca Just (List [ { match = "wesoły" , submatches = [] } ]) 
``` 
## Głębsza analiza

Regularne wyrażenia są rozwiązaniem bardzo wszechstronnym. Pierwotnie zostały stworzone w latach 50. XX wieku przez amerykańskiego matematyka Stephena Kleene'a. W obecnej wersji języka Elm wykorzystują one składnię JavaScript, a więc mogą być użyte w złożonych wzorcach. Alternatywnym rozwiązaniem jest również korzystanie z wbudowanego narzędzia rywalizującej biblioteki Posix.

## Zobacz też

Dla bardziej zaawansowanych zastosowań warto zapoznać się z modułem Regex.Experimental, który oferuje możliwość użycia wyrażeń regularnych w innym kontekście niż tekst. Można również znaleźć różne przydatne narzędzia do testowania i debugowania wyrażeń regularnych, takie jak regex101.com czy regtester.com.