---
title:    "Haskell: Pobieranie aktualnej daty"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

Często podczas pisania programów, potrzebujemy informacji o aktualnej dacie. Może to być do celów diagnostycznych, obliczeń czasu lub po prostu wyświetlania bieżącej daty dla użytkownika. W tym artykule dowiesz się, jak uzyskać aktualną datę w języku Haskell.

## Jak to zrobić

Aby uzyskać aktualną datę w Haskellu, musimy najpierw zaimportować moduł ```Data.Time```. W nim znajduje się funkcja ```getCurrentTime```, która zwraca bieżącą datę wraz z czasem.

```haskell
import Data.Time

getCurrentTime
```

Wywołanie funkcji ```getCurrentTime``` zwróci wartość typu ```UTCTime``` zawierającą datę i czas. Dzięki temu możemy wykorzystać funkcje z modułu ```Data.Time.Format```, aby sformatować datę do odpowiedniego formatu.

```haskell
import Data.Time
import Data.Time.Format

getCurrentTime >>= return . formatTime defaultTimeLocale "%d.%m.%Y"
```

Powyższy kod najpierw wywołuje funkcję ```getCurrentTime```, a następnie wykorzystuje operator ```>>=```, aby przekazać wynik do funkcji ```formatTime```. Używa też funkcji ```return```, aby wynik był typu ```IO String```, dzięki czemu możemy go wyświetlić w konsoli za pomocą funkcji ```putStrLn```.

Po uruchomieniu, otrzymamy bieżącą datę w formacie ```DD.MM.YYYY```.

```console
01.02.2021
```

## Deep Dive

Podczas pracy z datami w Haskellu, ważne jest aby zawsze używać typów specjalnych takich jak ```UTCTime``` lub ```Day```. Typ ```UTCTime``` jest używany do przechowywania daty i czasu w strefie czasowej UTC, natomiast typ ```Day``` tylko dla dat bez uwzględnienia czasu.

Ciekawym aspektem pracy z datami w Haskellu jest możliwość tworzenia instancji własnych typów, które będą reprezentować daty lub interwały czasowe. Wymaga to jednak trochę więcej wiedzy na temat pisania własnych typów i instancji. Więcej informacji na ten temat można znaleźć w oficjalnej dokumentacji języka Haskell.

## Zobacz także

- [Oficjalna dokumentacja języka Haskell](https://www.haskell.org/documentation/)
- [Moduł Data.Time](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Moduł Data.Time.Format](https://hackage.haskell.org/package/time/docs/Data-Time-Format.html)