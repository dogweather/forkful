---
title:                "Refaktoryzacja"
date:                  2024-01-26T03:37:48.167232-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refaktoryzacja"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/refactoring.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Refaktoryzacja to proces restrukturyzacji istniejącego kodu komputerowego bez zmiany jego zewnętrznych zachowań, mający na celu poprawę atrybutów niefunkcjonalnych oprogramowania. Programiści refaktoryzują kod, aby był on bardziej przejrzysty, wydajniejszy i łatwiejszy do zrozumienia, co ułatwia łatwiejsze utrzymanie i przyszłe ulepszenia.

## Jak to zrobić:
PowerShell nie ma wbudowanego, dedykowanego narzędzia do refaktoryzacji, ale nadal możesz oczyścić swój kod pod kątem czytelności i wydajności. Rozważ funkcję, która robi za dużo i jak możemy ją zrefaktoryzować dla większej jasności:

```PowerShell
function Get-InventoryData {
    # Oryginalna funkcja łącząca pobieranie danych i formatowanie
    $data = Get-Content -Path 'C:\inventory-list.txt'
    $inventoryData = $data | ForEach-Object {
        $fields = $_ -split ','
        [PSCustomObject]@{
            ItemID = $fields[0]
            Name   = $fields[1]
            Count  = $fields[2]
            Price  = $fields[3]
        }
    }
    $inventoryData | Format-Table -AutoSize
}

# Zrefaktoryzowana na oddzielne funkcje
function Import-InventoryData {
    param($Path)
    Get-Content -Path $Path | ForEach-Object {
        $fields = $_ -split ','
        [PSCustomObject]@{
            ItemID = $fields[0]
            Name   = $fields[1]
            Count  = $fields[2]
            Price  = $fields[3]
        }
    }
}

function Format-InventoryData {
    param($Data)
    $Data | Format-Table -AutoSize
}

# Użycie
$inventory = Import-InventoryData -Path 'C:\inventory-list.txt'
Format-InventoryData -Data $inventory
```

Przykładowe dane wyjściowe:

```
ItemID Name            Count Price
------ ----            ----- -----
1001   Widget Type A   50    9.99
1002   Gadget Type B   20    14.99
```

## Szczegółowa analiza
Refaktoryzacja w programowaniu ma korzenie sięgające najwcześniejszych dni rozwoju oprogramowania, choć została sformalizowana jako praktyka w latach 90. Książka Martina Fowlera "Refaktoryzacja: Udoskonalanie projektu istniejącego kodu" jest jednym z podstawowych dzieł na ten temat, podkreślająca znaczenie refaktoryzacji w osiąganiu czystego kodu.

Chociaż PowerShell nie jest wyposażony w specyficzne narzędzia do refaktoryzacji, tak jak zintegrowane środowiska programistyczne (IDE) dla innych języków (np. Eclipse czy Visual Studio), nadal można praktykować dobre zasady refaktoryzacji ręcznie. Kluczową rzeczą jest pamiętanie, że refaktoryzacja to nie tylko zmiana kodu dla samej zmiany, ale dokonywanie intencjonalnych, zachowujących zachowanie modyfikacji, które poprawiają strukturę i projekt kodu.

Alternatywy dla ręcznej refaktoryzacji w PowerShellu obejmują używanie IDE wspierających ten język, takich jak Visual Studio Code z rozszerzeniem PowerShell, które oferuje funkcje takie jak formatowanie kodu i podstawowe możliwości refaktoryzacji. Dla bardziej znaczącej refaktoryzacji możesz rozważyć wykorzystanie testów Pester, aby upewnić się, że zmiany nie zmieniają funkcjonalności.

Ponadto, implementacja refaktoryzacji może wiązać się z bardziej systemowymi zmianami, takimi jak modularizacja, gdzie kod jest dzielony na wielokrotnie używalne moduły lub funkcje, poprawiając przestrzeganie zasady DRY (Don't Repeat Yourself). Inne powszechne techniki refaktoryzacji obejmują zmianę nazw dla większej jasności, usuwanie zduplikowanego kodu i redukcję złożoności logiki warunkowej.

## Zobacz również
Aby zagłębić się głębiej, oto kilka zasobów:

- Książka Martina Fowlera o refaktoryzacji: [_Refaktoryzacja: Udoskonalanie projektu istniejącego kodu_](https://martinfowler.com/books/refactoring.html)
- Testowanie zrefaktoryzowanego kodu za pomocą Pester: [Framework testowy Pester](https://pester.dev/)
- Najlepsze praktyki PowerShell: [Przewodnik po najlepszych praktykach i stylu PowerShell](https://poshcode.gitbooks.io/powershell-practice-and-style/)
