---
title:                "Sprawdzanie, czy katalog istnieje"
html_title:           "Gleam: Sprawdzanie, czy katalog istnieje"
simple_title:         "Sprawdzanie, czy katalog istnieje"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Sprawdzanie, czy katalog istnieje, to zadanie polegające na weryfikacji, czy określony katalog istnieje w systemie plików. Programiści wykonują to, aby zapobiec błędom podczas próby odczytywania lub zapisywania plików z/do nieistniejącego katalogu.

## Jak to zrobić:

Obecnie, Gleam (najnowsza wersja) nie oferuje bezpośredniego sposobu na sprawdzenie, czy katalog istnieje. Najbliższą koncepcją jest używanie funkcji `fs.Dir.read` i zobaczenie, czy zwraca ona błąd. Oto przykładowy kod:

```Gleam
import gleam/fs.{Dir}

...

Dir.read("moj_folder")
|> result.is_error
```

Jeśli kod zwróci `True`, oznacza to, że katalog nie istnieje. W przeciwnym razie katalog istnieje.

## Deep Dive 

W Gleam, brak funkcji do bezpośredniego sprawdzania istnienia katalogu wynika z założeń języka, które preferują bezpieczne i przewidywalne operacje IO. Sprawdzanie, czy katalog istnieje, może prowadzić do problemów zwanych "race conditions", gdzie stan katalogu może zmienić się tuż po sprawdzeniu, co prowadzi do błędów.

Alternatywą jest przechwytywanie błędów podczas próby otwarcia lub zapisu do katalogu. Jest to bardziej bezpośredni sposób na zrozumienie, czy operacja jest możliwa.

Warto pamiętać, że Gleam jest językiem statycznie typowanym, więc część tej logiki jest obsługiwana na poziomie typów. Często nie musimy martwić się o te rzeczy, ponieważ typy a priori eliminują wiele potencjalnych błędów.

## Zobacz również 

Może Ci się przydać zapoznanie się z dokumentacją modułu `fs.Dir` na oficjalnej stronie Gleam. Odnajdziesz ją pod tym linkiem: [https://hexdocs.pm/gleam_stdlib/gleam/fs/dir.html](https://hexdocs.pm/gleam_stdlib/gleam/fs/dir.html).

Innym dobrym źródłem wiedzy na temat Gleam jest oficjalna strona języka: [https://gleam.run/](https://gleam.run/).

Niezależnie od powyższego, sprawdzanie istnienia katalogów jest powszechne w wielu językach programowania i funkcje obsługujące ten podzespół są często dostępne w pakietach standardowych.