---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:46.992939-07:00
description: "Parsowanie HTML w PowerShell to proces analizowania zawarto\u015Bci\
  \ HTML w celu wyodr\u0119bnienia konkretnych danych lub automatyzacji zada\u0144\
  \ zwi\u0105zanych z sieci\u0105.\u2026"
lastmod: '2024-03-13T22:44:35.627638-06:00'
model: gpt-4-0125-preview
summary: "Parsowanie HTML w PowerShell to proces analizowania zawarto\u015Bci HTML\
  \ w celu wyodr\u0119bnienia konkretnych danych lub automatyzacji zada\u0144 zwi\u0105\
  zanych z sieci\u0105."
title: "Analiza sk\u0142adniowa HTML"
weight: 43
---

## Co i dlaczego?
Parsowanie HTML w PowerShell to proces analizowania zawartości HTML w celu wyodrębnienia konkretnych danych lub automatyzacji zadań związanych z siecią. Programiści robią to, aby wchodzić w interakcje ze stronami internetowymi, pobierać zawartość sieciową lub automatyzować wysyłanie formularzy oraz inne interakcje sieciowe bez potrzeby korzystania z przeglądarki internetowej.

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
