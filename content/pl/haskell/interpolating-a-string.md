---
title:                "Interpolacja ciągów znakowych"
html_title:           "Haskell: Interpolacja ciągów znakowych"
simple_title:         "Interpolacja ciągów znakowych"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Co to jest interpolacja ciągu znaków?

Interpolacja ciągu znaków to proces wstawiania wartości zmiennych do tekstu. Jest to przydatne narzędzie dla programistów, ponieważ pozwala na tworzenie dynamicznych napisów, które mogą być dostosowywane w zależności od aktualnych danych.

## Jak to zrobić?

Możemy użyć operatora ```++``` lub funkcji ```concat``` aby połączyć ze sobą stałe i zmienne ciągi znaków. W poniższym przykładzie używamy operatora ```++```:

```Haskell
main = do
  let imie = "Kasia"
  let wiek = 25
  putStrLn ("Cześć, jestem " ++ imie ++ " i mam " ++ (show wiek) ++ " lat.")
```

Output:
Cześć, jestem Kasia i mam 25 lat.

## Wgląd w temat

Interpolacja ciągu znaków jest szeroko stosowanym narzędziem w wielu językach programowania, w tym w Haskellu. Alternatywą dla interpolacji jest użycie funkcji formatujących, jednakże interpolacja jest często preferowana ze względu na prostotę i czytelność kodu.

W implementacji, wartości zmiennych są najpierw konwertowane na ciągi znaków, a następnie wstawiane do odpowiedniego miejsca w tekście. W większości przypadków, interpolacja jest wykonywana podczas kompilacji, co przyspiesza działanie programu.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o interpolacji ciągu znaków w Haskellu, możesz zajrzeć na stronę dokumentacji języka lub przejrzeć inne przykładowe kody, aby lepiej zrozumieć zastosowanie tej techniki.