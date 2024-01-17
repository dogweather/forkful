---
title:                "Praca z JSON"
html_title:           "PowerShell: Praca z JSON"
simple_title:         "Praca z JSON"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/working-with-json.md"
---

{{< edit_this_page >}}

## W czym rzecz i dlaczego?

JSON to język znaczników, który jest często używany w programowaniu do przechowywania i przesyłania danych w formie tekstowej. Programiści często korzystają z niego, ponieważ jest prosty w użyciu i zapewnia wygodny sposób przechowywania oraz dostępu do informacji.

## Jak to zrobić?

```PowerShell
$json = '[{"imie": "Anna", "wiek": 25}, {"imie": "Jan", "wiek": 32}]'

# Wypisanie danych w postaci tabeli
$json | ConvertFrom-Json | Format-Table -Property imie, wiek

# Wypisanie danych w postaci listy
$json | ConvertFrom-Json | ForEach-Object { "{0} ma {1} lat." -f $_.imie, $_.wiek }
```

Wynik:

imie | wiek
-----|-----
Anna | 25
Jan  | 32

Anna ma 25 lat.
Jan ma 32 lata.

## Głębsze wgląd

JSON został opracowany w latach 90. jako sposób na przesyłanie danych między aplikacjami internetowymi, jako alternatywa dla wcześniej stosowanego XML. W PHP jest często używany do formatowania danych w formie tablicy asocjacyjnej. W przypadku programu PowerShell, korzystamy z modułu ```ConvertFrom-Json```, który pozwala nam na przetworzenie danych w formacie JSON do wygodniejszych dla nas form.

## Zobacz też

- Dokumentacja modułu ```ConvertFrom-Json```: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/convertfrom-json
- Przykładowa strona z danymi w formacie JSON: https://jsonplaceholder.typicode.com/users