---
title:                "Tworzenie tymczasowego pliku"
html_title:           "C#: Tworzenie tymczasowego pliku"
simple_title:         "Tworzenie tymczasowego pliku"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Tworzenie tymczasowego pliku polega na utworzeniu pliku o określonym czasie życia, używanego do przechowywania informacji przez krótki okres czasu. Programiści korzystają z plików tymczasowych do zapisaniu danych pośrednich, które mogą być potrzebne później w trakcie wykonywania programu.

## Jak:

Poniżej przedstawię przykład tworzenia pliku tymczasowego w PowerShell:
```PowerShell
# Tworzenie nazwy pliku
$tempFile = [System.IO.Path]::GetTempFileName()

# Zapis danych do pliku
Set-Content -Path $tempFile -Value "Awokado z grilla"

# Wyświetlanie zawartości pliku
Get-Content -Path $tempFile
```
To powyższe polecenia utworzą plik tymczasowy i zapiszą do niego tekst "Awokado z grilla". Następnie wyświetli zawartość pliku.

## Deep Dive:

Pliki tymczasowe są używane od początków programowania komputerowego. Powstają jako odpowiedź na potrzebę przechowywania danych między różnymi operacjami lub sesjami. Pozwalają na przechowywanie dużych ilości danych, które nie muszą być przechowywane na stałe.

Jeśli chodzi o alternatywy, wykorzystanie pamięci RAM do przechowywania danych tymczasowych może być szybsze, ale jest ograniczone przez dostępną ilość pamięci.

W PowerShell, funkcja [System.IO.Path]::GetTempFileName() tworzy unikalne pliki tymczasowe. Każdy utworzony plik otrzymuje unikalną nazwę, co zapobiega kolizji z innymi plikiem tymczasowym. 

## See Also:

- Dokumentacja Microsoft odnośnie do funkcji GetTempFileName(): [System.IO.Path.GetTempFileName Method](https://docs.microsoft.com/en-us/dotnet/api/system.io.path.gettempfilename?view=net-5.0)
- Alternatywa dla plików tymczasowych - Pamięci RAM: [RAM as a storage in computing](https://en.wikipedia.org/wiki/Random-access_memory)