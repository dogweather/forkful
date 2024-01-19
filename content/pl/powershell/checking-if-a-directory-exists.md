---
title:                "Sprawdzanie, czy katalog istnieje"
html_title:           "Go: Sprawdzanie, czy katalog istnieje"
simple_title:         "Sprawdzanie, czy katalog istnieje"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Sprawdzanie, czy katalog istnieje, to proces identyfikowania, czy określony katalog jest obecny w systemie plików lub nie. Programiści wykonują to, aby uniknąć błędów podczas pracy z plikami lub katalogami, które mogą nie istnieć.

## Jak zrobić:
Możemy sprawdzić, czy katalog istnieje w PowerShell, za pomocą polecenia `Test-Path`. Oto przykład:

```PowerShell
if(Test-Path $path) 
{
   Write-Output "Katalog istnieje"
} 
else 
{
   Write-Output "Katalog nie istnieje"
}
```
Jeśli katalog istnieje, wyświetli "Katalog istnieje", w przeciwnym razie wyświetli "Katalog nie istnieje".

## Deep Dive
1. Kontekst historyczny: W przeszłości, przed narodzinami PowerShell, sprawdzanie istnienia katalogu było trudniejsze i wymagało więcej linii kodu. PowerShell uprościł ten proces dzięki swoim wbudowanym poleceniom.
2. Alternatywy: Możemy również korzystać z .NET Framework do sprawdzenia istnienia katalogu. Poniżej prezentujemy przykład:

```PowerShell
if([System.IO.Directory]::Exists($path)) 
{
   Write-Output "Katalog istnieje"
} 
else 
{
   Write-Output "Katalog nie istnieje"
}
```

3. Szczegóły implementacji: Polecenie `Test-Path` zwraca prawdę (True) lub fałsz (False) w zależności od istnienia katalogu. Możemy go użyć do zabezpieczenia naszego kodu przed próbą interakcji z nieistniejącym katalogiem.

## Zobacz też:
1. [Dokumentacja PowerShell Test-Path](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/test-path?view=powershell-6)
2. [Dokumentacja .NET Framework Directory.Exists](https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists?view=net-5.0)
3. [Tutorial do PowerShell dla początkujących](https://docs.microsoft.com/pl-pl/powershell/scripting/overview?view=powershell-7.1)