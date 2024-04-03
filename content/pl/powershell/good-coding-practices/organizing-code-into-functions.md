---
date: 2024-01-26 01:11:16.578349-07:00
description: "Jak to zrobi\u0107: Napiszmy funkcj\u0119 do obliczania sumy dw\xF3\
  ch liczb. Proste, ale ilustruje to, o co chodzi."
lastmod: '2024-03-13T22:44:35.635379-06:00'
model: gpt-4-1106-preview
summary: "Napiszmy funkcj\u0119 do obliczania sumy dw\xF3ch liczb."
title: Organizacja kodu w funkcje
weight: 18
---

## Jak to zrobić:
Napiszmy funkcję do obliczania sumy dwóch liczb. Proste, ale ilustruje to, o co chodzi.

```PowerShell
function Dodaj-Liczby {
    param (
        [int]$PierwszaLiczba,
        [int]$DrugaLiczba
    )
    return $PierwszaLiczba + $DrugaLiczba
}

# Wywołanie funkcji z liczbami 5 i 10
$suma = Dodaj-Liczby -PierwszaLiczba 5 -DrugaLiczba 10
Write-Output "Suma wynosi $suma"
```

Przykładowe wyjście:

```
Suma wynosi 15
```

## Dogłębna analiza
Funkcje w PowerShellu, jak w większości języków, nie są nowością. Kod kompartmentalizujemy od czasów Fortranu. Chodzi o 'nie wynalezanie koła na nowo'. Alternatywy? Pewnie, skrypty lub polecenia cmdlet. Ale brakuje im schludności i kontekstowej czułości funkcji w skryptach.

Implementacja? Funkcje mogą być podstawowe jak w naszym przykładzie lub złożone ze zmiennymi zasięgami, wejściem potokowym i więcej. Weźmy na przykład 'Zaawansowane funkcje'. Naśladują cmdlety z parametrami mającymi atrybuty, takie jak `[Parameter(Mandatory=$true)]`. To tylko przykład elastyczności PowerShellu.

## Zobacz także
- [about_Functions_Advanced_Parameters](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_functions_advanced_parameters?view=powershell-7.1)
- [about_Script_Blocks](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_script_blocks?view=powershell-7.1)
