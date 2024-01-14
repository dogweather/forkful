---
title:    "Elixir: Używanie wyrażeń regularnych"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Dlaczego warto poznać wyrażenia regularne w Elixir

Wyrażenia regularne są niezwykle przydatnym narzędziem w programowaniu, pozwalającym na wyszukiwanie i manipulację tekstem w prosty i wydajny sposób. W Elixir mają one szczególne znaczenie, ponieważ są często używane w funkcjach wbudowanych oraz w przypadku parsowania danych wejściowych. Poznanie wyrażeń regularnych w Elixir może znacznie ułatwić prace z tekstem oraz zwiększyć wydajność kodu.

## Jak używać wyrażeń regularnych w Elixir

Możesz używać wyrażeń regularnych w Elixir za pomocą wbudowanej funkcji `Regex`. Poniżej znajdują się przykładowe kody wykorzystujące wyrażenia regularne w celu wyszukania i zamiany tekstu:

```Elixir
str = "To jest przykład tekstu do przetworzenia"
Regex.replace(~r/przykład/, str, "próba")
# Wynik: "To jest próba tekstu do przetworzenia"

Regex.scan(~r/[a-z]+/, str)
# Wynik: ["To", "jest", "przykład", "tekstu", "do", "przetworzenia"]
```

W powyższych przykładach użyto funkcji `Regex.replace/3`, która zastępuje dopasowane wyrażenie regularne podanym tekstem oraz funkcji `Regex.scan/2`, która zwraca listę dopasowanych wyrażeń. Istnieje wiele innych funkcji wbudowanych, które można wykorzystać, aby pracować z wyrażeniami regularnymi w Elixir.

## Głębszy wgląd w wyrażenia regularne

Wyrażenia regularne w Elixir są oparte na standardzie PCRE (Perl-Compatible Regular Expressions) oraz oferują szereg wygodnych dodatków, takich jak obsługa Unicode i możliwość zastąpienia tekstu funkcją. Warto poświęcić czas na poznanie składni wyrażeń regularnych oraz dostępnych funkcji, aby móc wykorzystać je w sposób jak najbardziej efektywny.

# Zobacz również

- [Dokumentacja Elixir dla funkcji Regex](https://hexdocs.pm/elixir/Regex.html)
- [Poradnik składni wyrażeń regularnych w Elixir](https://www.rexwolf.net/2016/06/elixir-regular-expressions/)
- [Przykłady użycia wyrażeń regularnych w Elixir](https://csainty.github.io/elixir/2017/10/05/elixir-regular-expressions.html)