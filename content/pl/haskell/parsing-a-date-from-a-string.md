---
title:                "Analiza składniowa daty z ciągu znaków"
html_title:           "Clojure: Analiza składniowa daty z ciągu znaków"
simple_title:         "Analiza składniowa daty z ciągu znaków"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?

Przetwarzanie daty z ciągu znaków to proces zamiany tekstowej reprezentacji daty (np. "20-12-2022") na odpowiednią strukturę danych. Programiści robią to, aby manipulować datami i czasem w bardziej wymiarowy sposób.

## Jak to zrobić:

Haskell oferuje nam wiele możliwości do parsowania dat. Możemy skorzystać z biblioteki `Data.Time` i użyć funkcji `parseTimeM`. Oto, jak to zrobić:

```Haskell
import Data.Time
import Data.Time.Format

main = do
  let dateString = "2022-12-20"
  let format = "%Y-%m-%d"
  let maybeDate = parseTimeM True defaultTimeLocale format dateString
  print (maybeDate :: Maybe Day)
```

Po uruchomieniu tego kodu otrzymasz taki wynik:

```Haskell
Just 2022-12-20
```

## Deep Dive

Jeżeli mówimy o przetwarzaniu dat z ciągu znaków, warto znać historię. Analiza danych była od zawsze ważnym elementem programowania, przede wszystkim dlatego, że dane wejściowe są często postrzegane jako ciągi znaków, a parsowanie pozwala na ich łatwe użycie. 

Mniejsze projekty mogą skorzystać z innych funkcji do przekształcenia ciągu znaków w datę, takich jak `read` i `show`, ale nie oferują one takiej elastyczności jak `parseTimeM`. 

`parseTimeM` pochodzi z biblioteki `Data.Time`. To bardzo wydajne narzędzie, które daje nam dużo opcji, ale wszystko sprowadza się do Tru, False, formule formatu i ciągu.
  
## Zobacz Też:

- [Dokumentacja biblioteki Data.Time](http://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)
- [Poradnik o parsowaniu dat w Haskellu](https://riptutorial.com/haskell/example/5805/parsing-a-date-and-time)