---
title:                "Przetwarzanie HTML"
date:                  2024-01-20T15:33:26.727780-07:00
simple_title:         "Przetwarzanie HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?
(## Co i dlaczego?)
Parsowanie HTML to proces wydobywania danych ze struktur stron internetowych. Programiści robią to, żeby automatycznie pobierać informacje, przetwarzać treści lub monitorować zmiany na stronach.

## How to:
(## Jak to zrobić:)
Do parsowania HTML w PowerShell możemy użyć Invoke-WebRequest i HtmlAgilityPack. Oto przykład:

```PowerShell
# Należy pamiętać, że HtmlAgilityPack wymaga instalacji z NuGet
# Install-Package HtmlAgilityPack

# Pobranie strony
$response = Invoke-WebRequest -Uri 'http://example.com'

# Załadowanie HTML do HtmlAgilityPack
$htmlDoc = New-Object HtmlAgilityPack.HtmlDocument
$htmlDoc.LoadHtml($response.Content)

# Wydobycie danych
# Załóżmy, że chcemy wszystkie nagłówki h1 ze strony
$headers = $htmlDoc.DocumentNode.SelectNodes('//h1')
foreach($header in $headers) {
    Write-Output $header.InnerText
}
```
Przykładowa odpowiedź:
```
Witaj na Example.com!
Inny przykładowy nagłówek
```

## Deep Dive:
(## Szczegółowe informacje:)
W przeszłości dane często scrapowano za pomocą wyrażeń regularnych, ale to ryzykowna i niewystarczająco elastyczna metoda. HtmlAgilityPack, biblioteka .NET pozwalająca na skomplikowane zapytania XPath i manipulacje DOM, jest lepszym wyborem. Alternatywą jest AngleSharp, kolejna biblioteka .NET, która świetnie radzi sobie z parsingiem HTML5. Mimo że PowerShell natywnie nie ma mocnych narzędzi do parsowania HTML, korzystając z tych bibliotek, można łatwo przetwarzać strony i wydobywać potrzebne dane.

## See Also:
(## Zobacz też:)
- Dokumentacja HtmlAgilityPack: https://html-agility-pack.net/
- Informacje o poleceniu Invoke-WebRequest: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest
- AngleSharp GitHub Repo: https://github.com/AngleSharp/AngleSharp
- Poradnik do XPath: https://www.w3schools.com/xml/xpath_intro.asp
