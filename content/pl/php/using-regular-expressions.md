---
title:                "Wykorzystanie wyrażeń regularnych"
date:                  2024-01-19
html_title:           "Arduino: Wykorzystanie wyrażeń regularnych"
simple_title:         "Wykorzystanie wyrażeń regularnych"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Wyrażenia regularne to sposób wyszukiwania i manipulowania tekstami. Programiści używają ich dla szybkiej i elastycznej obsługi ciągów znaków, np. do walidacji danych, wyszukiwania wzorców czy masowej edycji.

## Jak to zrobić?
```PHP
<?php
$tekst = "Kontakt: email@example.com";
$wzorzec = "/[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,6}/";

if (preg_match($wzorzec, $tekst, $wyniki)) {
    echo "Znaleziony adres email: " . $wyniki[0];
} else {
    echo "Adres email nie został znaleziony.";
}
?>
```
Wynik: `Znaleziony adres email: email@example.com`

## Deep Dive
Wyrażenia regularne powstały w latach 50. i od zawsze były kluczową częścią uniwersalnych edytorów tekstu oraz języków programowania. Alternatywą dla nich są parser’y, które jednak często wymagają bardziej skomplikowanego kodu. W PHP używa się składni PCRE (Perl Compatible Regular Expressions), która dostarcza bogaty zestaw funkcji, jak `preg_match()`, `preg_replace()` czy `preg_split()`.

## Zobacz też
- [Tutorial wyrażeń regularnych](https://www.regular-expressions.info/tutorial.html)
- [Online tester wyrażeń regularnych](https://regex101.com/)
