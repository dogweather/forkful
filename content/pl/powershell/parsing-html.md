---
title:                "Odczytywanie html"
html_title:           "PowerShell: Odczytywanie html"
simple_title:         "Odczytywanie html"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/parsing-html.md"
---

{{< edit_this_page >}}

Cześć programiści!

Jeśli kiedykolwiek próbowaliście pobierać informacje z internetu, na pewno spotkaliście się z językiem HTML. Jest to język używany do tworzenia stron internetowych i składający się z różnych znaczników, atrybutów i wartości. Ale jak można wykorzystać te informacje w programowaniu? Tutaj właśnie przychodzi z pomocą parsing HTML w PowerShell.

## Co to jest i po co to robić?

Parsing HTML to proces analizowania kodu HTML w celu pobrania interesujących nas informacji. Dzięki temu możemy w prosty sposób pobierać i przetwarzać informacje z różnych stron internetowych bez potrzeby przepisywania całego kodu HTML. Jest to szczególnie przydatne, gdy chcemy przetwarzać dużą ilość danych.

## Jak to zrobić?

```PowerShell
$url = "https://www.example.com"
$response = Invoke-WebRequest -Uri $url #pobieranie zawartości strony
$parsedHTML = [System.Xml.XmlDocument]$response.Content #parsowanie kodu HTML
$targetElement = $parsedHTML.selectSingleNode("//div[@class='example']") #wyszukiwanie elementu o określonym atrybucie
$targetElement.InnerText #wyświetlenie zawartości elementu
```

Powyższy kod wyświetli zawartość elementu o klasie "example" ze strony o podanym adresie. Dzięki temu możemy w łatwy sposób pobierać tylko te informacje, które nas interesują.

## Głębsza analiza

Parsing HTML jest procesem używanym od dawna w programowaniu, gdyż jest niezbędny do pobierania i przetwarzania informacji z internetu. Alternatywą dla parsingu HTML może być na przykład scrapping, jednak jest to bardziej złożone i nie zawsze możliwe. W PowerShell istnieje również moduł "HTMLAgilityPack", który ułatwia parsing kodu HTML.

Jeśli chcesz dowiedzieć się więcej o funkcjach dostępnych przy użyciu parsingu HTML w PowerShell, polecam zapoznać się z oficjalną dokumentacją: https://docs.microsoft.com/en-us/previous-versions/windows/it-pro/windows-powershell-1.0/ee156811(v=technet.10)

## Zobacz także

Jeśli interesuje Cię temat pobierania informacji z internetu, warto również poznać bibliotekę "Invoke-Webrequest", która jest często używana razem z parsingiem HTML: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-7

Mam nadzieję, że ten krótki artykuł pomógł Ci zrozumieć, czym jest parsing HTML w PowerShell i jak wykorzystać go w codziennej pracy. Powodzenia w programowaniu!