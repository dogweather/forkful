---
title:                "Pobieranie strony internetowej"
html_title:           "C#: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Pobieranie Stron Internetowych za pomocą PowerShell

## Co i dlaczego?

Pobieranie stron internetowych to proces zapisywania ich treści lokalnie. Programiści robią to przede wszystkim dla przetwarzania danych (web scraping), testowania lub archiwizacji.

## Jak to zrobić:

W PowerShell możemy to zrobić bardzo prosto. Poniżej znajduje się przykładowy kod:

```PowerShell
$url = "http://przyklad.pl"
$response = Invoke-WebRequest -Uri $url
$response.Content | Out-File -FilePath C:\ścieżka\do\pliku.html
```

Wynik tego kodu to zapisanie całej strony http://przyklad.pl jako plik HTML na dysku lokalnym.

## Więcej szczegółów:

Pobieranie stron internetowych to stara praktyka, która istnieje od początków internetu. PowerShell z winęłością .NET oferuje dużą elastyczność w tej materii.

Alternatywą jest korzystanie z innych narzędzi, takich jak wget w Linux. Można również stosować specjalistyczne biblioteki w językach programowania, takie jak 'requests' w Python.

Podczas korzystania z PowerShell warto pamiętać o pewnych szczegółach implementacyjnych: 
- `Invoke-WebRequest` korzysta z sesji web, co pozwala na obsługę ciasteczek i autentykacji.
- Aby obsługiwać strony z dużą ilością treści JS, można potrzebować dodatkowych narzędzi, takich jak Selenium.

## Zobacz też:

1. Szerokie omówienie `Invoke-WebRequest`: [Link](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-7.1)
3. Obsługa stron z dużą ilością JavaScriptu: [Link](https://stackoverflow.com/questions/46372116/powershell-invoke-webrequest-and-javascript)