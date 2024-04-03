---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:46.992939-07:00
description: "Jak to zrobi\u0107: PowerShell nie posiada natywnie dedykowanego analizatora\
  \ HTML, ale mo\u017Cna wykorzysta\u0107 cmdlet `Invoke-WebRequest` do dost\u0119\
  pu i analizowania\u2026"
lastmod: '2024-03-13T22:44:35.627638-06:00'
model: gpt-4-0125-preview
summary: "PowerShell nie posiada natywnie dedykowanego analizatora HTML, ale mo\u017C\
  na wykorzysta\u0107 cmdlet `Invoke-WebRequest` do dost\u0119pu i analizowania tre\u015B\
  ci HTML."
title: "Analiza sk\u0142adniowa HTML"
weight: 43
---

## Jak to zrobić:
PowerShell nie posiada natywnie dedykowanego analizatora HTML, ale można wykorzystać cmdlet `Invoke-WebRequest` do dostępu i analizowania treści HTML. Dla bardziej złożonego parsowania i manipulacji, można zastosować HtmlAgilityPack, popularną bibliotekę .NET.

### Korzystanie z `Invoke-WebRequest`:
```powershell
# Prosty przykład pobierania tytułów ze strony internetowej
$response = Invoke-WebRequest -Uri 'http://example.com'
# Wykorzystanie właściwości ParsedHtml do dostępu do elementów DOM
$title = $response.ParsedHtml.title
Write-Output $title
```

Przykładowe wyjście:

```
Przykładowa domena
```

### Korzystanie z HtmlAgilityPack:
Najpierw musisz zainstalować HtmlAgilityPack. Można to zrobić przez Menedżera Pakietów NuGet:

```powershell
Install-Package HtmlAgilityPack -ProviderName NuGet
```

Następnie możesz użyć go w PowerShell do analizowania HTML:

```powershell
# Załaduj zestaw HtmlAgilityPack
Add-Type -Path "ścieżka\do\HtmlAgilityPack.dll"

# Stwórz obiekt HtmlDocument
$doc = New-Object HtmlAgilityPack.HtmlDocument

# Wczytaj HTML z pliku lub zapytania sieciowego
$htmlContent = (Invoke-WebRequest -Uri "http://example.com").Content
$doc.LoadHtml($htmlContent)

# Użyj XPath lub innych metod zapytań do wyodrębnienia elementów
$node = $doc.DocumentNode.SelectSingleNode("//h1")

if ($node -ne $null) {
    Write-Output $node.InnerText
}
```

Przykładowe wyjście:

```
Witaj na Example.com!
```

W tych przykładach, `Invoke-WebRequest` najlepiej nadaje się do prostych zadań, podczas gdy HtmlAgilityPack oferuje znacznie bogatszy zestaw funkcji do złożonego analizowania i manipulowania kodem HTML.
