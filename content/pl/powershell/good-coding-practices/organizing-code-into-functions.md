---
title:                "Organizacja kodu w funkcje"
aliases: - /pl/powershell/organizing-code-into-functions.md
date:                  2024-01-26T01:11:16.578349-07:00
model:                 gpt-4-1106-preview
simple_title:         "Organizacja kodu w funkcje"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/organizing-code-into-functions.md"
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
