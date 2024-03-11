---
date: 2024-01-26 01:11:16.578349-07:00
description: "Organizowanie kodu w funkcje polega na grupowaniu fragment\xF3w kodu,\
  \ kt\xF3re wykonuj\u0105 okre\u015Blone zadania i nadawaniu im nazwy. Robi si\u0119\
  \ to, aby kod by\u0142\u2026"
lastmod: '2024-03-11T00:14:08.829557-06:00'
model: gpt-4-1106-preview
summary: "Organizowanie kodu w funkcje polega na grupowaniu fragment\xF3w kodu, kt\xF3\
  re wykonuj\u0105 okre\u015Blone zadania i nadawaniu im nazwy. Robi si\u0119 to,\
  \ aby kod by\u0142\u2026"
title: Organizacja kodu w funkcje
---

{{< edit_this_page >}}

## Co i dlaczego?

Organizowanie kodu w funkcje polega na grupowaniu fragmentów kodu, które wykonują określone zadania i nadawaniu im nazwy. Robi się to, aby kod był wielokrotnego użytku, czytelny i łatwy w utrzymaniu. Zamiast przepisywać ten sam kod, wywołaj funkcję. Chcesz dokonać rozwiązywania problemów lub ulepszyć? Zmodyfikuj funkcję bez przekopywania się przez stosy skryptów.

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
