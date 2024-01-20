---
title:                "Generowanie liczb losowych"
html_title:           "Gleam: Generowanie liczb losowych"
simple_title:         "Generowanie liczb losowych"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Generowanie liczb losowych to proces tworzenia liczb, które nie mają żadnego przewidywalnego wzorca. Programiści robią to dla wielu celów, takich jak zabezpieczenia, symulacje czy testowanie.

## Jak to zrobić:
PowerShell posiada wbudowany mechanizm do generowania liczb losowych. Załóżmy, że chcemy wygenerować losową liczbę między 1 a 100.

```PowerShell
Get-Random -Minimum 1 -Maximum 100
```

Po wykonaniu tego kodu, na ekranie zobaczysz losową liczbę w tym zakresie.

Jeśli chcesz wygenerować listę 10 losowych liczb, możesz to zrobić w następujący sposób:

```PowerShell
1..10 | ForEach-Object { Get-Random -Minimum 1 -Maximum 100 }
```
Wynik tego kodu to 10 losowych liczb między 1 a 100.

## Pogłębione informacje:
Generowanie liczb losowych było częścią programowania od początku jego istnienia. Zainteresowanie tym tematem zaczęło wzrastać w związku z coraz większym zapotrzebowaniem na bezpieczeństwo i prywatność w sieci.

Jedną z alternatyw dla `Get-Random` jest użycie klasy `System.Random` w .NET, co oferuje dodatkowe funkcje, takie jak generowanie liczb losowych z określonego rozkładu.

W przypadku `Get-Random`, PowerShell korzysta z generatora liczb pseudolosowych z klasy `System.Security.Cryptography.RNGCryptoServiceProvider` w .NET, który jest bardziej odpowiedni dla zastosowań wymagających większej losowości i bezpieczeństwa niż podstawowy generator `System.Random`.

## Zobacz też:
1. [Dokumentacja Get-Random na Microsoft Docs](https://docs.microsoft.com/pl-pl/powershell/module/microsoft.powershell.utility/get-random?view=powershell-7.1)
2. [Dokumentacja System.Random na Microsoft Docs](https://docs.microsoft.com/pl-pl/dotnet/api/system.random?view=net-5.0)
3. [Dokumentacja System.Security.Cryptography.RNGCryptoServiceProvider na Microsoft Docs](https://docs.microsoft.com/pl-pl/dotnet/api/system.security.cryptography.rngcryptoserviceprovider?view=net-5.0)