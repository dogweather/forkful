---
title:                "Pobieranie strony internetowej"
html_title:           "PowerShell: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

Co i Dlaczego?

Pobieranie strony internetowej to proces odczytywania i zapisywania zawartości strony internetowej do lokalnego komputera. Programiści często pobierają strony internetowe w celu analizy zawartości lub wykorzystania jej w swoich programach.

Jak to zrobić:

```PowerShell
$client = New-Object System.Net.WebClient
$url = "https://www.example.com"
$path = "C:\Users\Example\Website.html"
$client.DownloadFile($url,$path)
```

W powyższym przykładzie jest tworzony nowy obiekt WebClient, który jest następnie używany do pobrania zawartości strony internetowej poprzez podanie adresu URL oraz ścieżki do zapisania pliku na lokalnym komputerze.

Scala do głębszych wód:

1. Kontekst historyczny: Pobieranie stron internetowych jest powszechną praktyką w programowaniu od czasów początku Internetu.
2. Alternatywy: Oprócz użycia PowerShell, możliwe jest również pobranie zawartości strony internetowej przy użyciu innych języków programowania takich jak Python czy Java.
3. Szczegóły implementacyjne: W przypadku bardziej skomplikowanych stron internetowych, które wymagają logowania lub obsługi ciasteczek, może być konieczne użycie specjalnych modułów lub bibliotek dla PowerShell.

Zobacz też:

- Dokumentacja dla klasy WebClient w PowerShell: https://docs.microsoft.com/en-us/powershell/module/Microsoft.PowerShell.Utility/Invoke-WebRequest?view=powershell-7.1
- Przykłady pobierania stron internetowych z użyciem innych języków programowania: https://realpython.com/python-web-scraping-practical-introduction/