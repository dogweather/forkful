---
title:                "Odczytanie argumentów wiersza poleceń"
html_title:           "PowerShell: Odczytanie argumentów wiersza poleceń"
simple_title:         "Odczytanie argumentów wiersza poleceń"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Czym jest czytanie argumentów wiersza poleceń i dlaczego programiści to robią

Czytanie argumentów wiersza poleceń, znane także jako parametry lub opcje, polega na pobieraniu informacji przekazanych do programu podczas jego uruchamiania. Jest to ważna umiejętność dla każdego programisty, ponieważ pozwala na interakcję użytkownika z programem, dostosowywanie jego zachowania i wykorzystywanie go w różnych środowiskach.

# Jak to zrobić?

```PowerShell
# Proste pobieranie argumentów wiersza poleceń
$args = $args # Zmiennej $args przypisujemy wszystkie argumenty wiersza poleceń

# Przetwarzanie argumentów wiersza poleceń za pomocą pętli
foreach ($arg in $args) {
    # Kod przetwarzania argumentów
    $arg
}

# Ustawianie nazw wartości dla argumentów wiersza poleceń
My-Program -Parametr1 Wartość1 -Parametr2 Wartość2
```

Przykłady wywołania powyższego programu w wierszu poleceń:

```bash
powershell.exe my-program.ps1 argument1 argument2
powershell.exe my-program.ps1 -Parametr1 Wartość1 -Parametr2 Wartość2
```

# Głębsze zagadnienia

## Kontekst historyczny

Czytanie argumentów wiersza poleceń jest powszechną praktyką w świecie programowania, zwłaszcza w przypadku języków skryptowych i narzędzi wykorzystywanych w systemach operacyjnych. Jest to wygodny sposób na konfigurację programu bez konieczności jego modyfikacji.

## Alternatywy

Jedną z alternatyw dla czytania argumentów wiersza poleceń jest wykorzystanie pliku konfiguracyjnego lub graficznego interfejsu użytkownika. Jednak korzystanie z argumentów wiersza poleceń jest bardziej przystępne dla programistów i może być wykorzystywane w prostych skryptach.

## Szczegóły implementacji

W języku PowerShell, argumenty wiersza poleceń są przechowywane w zmiennej $args, która jest tablicą zawierającą wszystkie argumenty wprowadzone do programu. Można również użyć specjalnej składni z dwukropkami, aby ustawić nazwy wartości dla argumentów.

# Zobacz również

- [Dokumentacja o argumentach wiersza poleceń w PowerShell](https://docs.microsoft.com/pl-pl/powershell/module/microsoft.powershell.core/about/about_functions_advanced_parameters?view=powershell-7)
- [Poradnik dla programistów: Rozpoznawanie argumentów wiersza poleceń](https://adamtheautomator.com/wp-content/uploads/2017/05/Understanding-Command-Line-Arguments-The-Simple-Way.pdf)
- [Wideo: Jak czytać argumenty wiersza poleceń w PowerShell](https://www.youtube.com/watch?v=k_ZomH1-Cxo)