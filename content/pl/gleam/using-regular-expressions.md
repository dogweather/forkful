---
title:                "Gleam: Używanie wyrażeń regularnych"
simple_title:         "Używanie wyrażeń regularnych"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Dlaczego

Wykorzystanie wyrażeń regularnych jest niezbędne w wielu dziedzinach programowania, zwłaszcza przy analizie i manipulacji tekstem. Jest to niezwykle przydatne narzędzie do przetwarzania i wyszukiwania danych w różnych formatach, a także ułatwiające pracę z dużymi zbiorami danych. W artykule dowiesz się, dlaczego warto nauczyć się korzystać z wyrażeń regularnych w języku programowania Gleam.

## Jak wykorzystać wyrażenia regularne w Gleam

Aby rozpocząć pracę z wyrażeniami regularnymi w Gleam, należy zaimportować pakiet "tobby/regex" za pomocą polecenia ```import "tobby/regex"```. Następnie można użyć funkcji ```Regex.new()``` do utworzenia wyrażenia regularnego, a następnie ```Regex.match()``` do przeszukiwania tekstu i zwrócenia dopasowań. Poniższy przykład ilustruje wykorzystanie wyrażeń regularnych do odnalezienia powtarzających się słów w zdaniu:

```Gleam
import "tobby/regex"

let sentence = "To jest przykładowe zdanie z powtarzającymi się słowami. To jest przykładowe zdanie."
let regex = Regex.new("(?i)\\b(\\w+)\\b\\s+\\b\\1\\b")
let matches = Regex.match(regex, sentence)

// samples zawiera listę ["To", "jest"] jako dopasowane słowa
```

Innym przydatnym narzędziem jest funkcja ```Regex.replace()```, która pozwala na zamianę dopasowanych fragmentów tekstu na wybrane wyrażenie. Przykładowo, można przeformatować daty zapisane w niejednolitym formacie:

```Gleam
let dates = "01-01-2020, 01.02.2021, 01/03/2022"
let regex = Regex.new("(\\d{2})[./-](\\d{2})[./-](\\d{4})")
let formatted_dates = Regex.replace(regex, dates, "YYYY-MM-DD")

// wynikiem będzie "2020-01-01, 2021-02-01, 2022-03-01"
```

## Zagłębienie się w wyrażenia regularne

Wyrażenia regularne oferują wiele możliwości, a z czasem można nauczyć się coraz bardziej skomplikowanych wzorców i wykorzystywać dodatkowe funkcje i składnię. W języku Gleam dostępne są także inne pakiety, takie jak "tobby/peg" czy "evadne/lingua" oferujące jeszcze więcej funkcjonalności związanych z wyrażeniami regularnymi.

## Zobacz także

- Dokumentacja Gleam: https://gleam.run/
- Przewodnik po wyrażeniach regularnych: https://regexone.com/
- Wyrażenia regularne w języku Gleam: https://gleam.toby.cloud/tobby/regex/latest/Regex/
- Inne przydatne pakiety Gleam: https://github.com/gleam-lang/awesome-gleam