---
title:                "Interpolacja ciągu znaków"
html_title:           "PowerShell: Interpolacja ciągu znaków"
simple_title:         "Interpolacja ciągu znaków"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## O co chodzi i dlaczego?

Jeśli jesteście programistami lub pracujecie z językami programowania, na pewno słyszeliście o interpolacji stringów. Czym dokładnie jest ta operacja i po co ją stosujemy? Interpolacja polega na łączeniu wartości zmiennych lub wyrażeń z danym tekstem, tworząc w ten sposób nowy string. Jest to przydatne w przypadku, gdy chcemy dynamicznie dostosować tekst do aktualnych warunków, bez konieczności ciągłej zmiany całego kodu.

## Jak to zrobić?

Interpolacja stringów w PowerShell jest bardzo prosta – wystarczy użyć znaku dolara ($) przed nazwą zmiennej lub wyrażenia wewnątrz cudzysłowu (""). Przykładowo:

```PowerShell
$imie = "Jan"
"Hello, $imie" # wynik: Hello, Jan
```

Warto zauważyć, że interpolacja działa również wewnątrz innych znaków, takich jak apostrofy czy nawiasy kwadratowe. Możemy także łączyć kilka zmiennych lub wyrażeń, korzystając z operatora plus (+):

```PowerShell
$imie = "Jan"
$nazwisko = "Kowalski"
"Hello, $imie $nazwisko" # wynik: Hello, Jan Kowalski
"Pierwsza litera twojego imienia to $($imie[0])" # wynik: Pierwsza litera twojego imienia to J
"Twoje inicjały to $imie[0]. $nazwisko[0]." # wynik: Twoje inicjały to J. K.
```

Oczywiście, możemy także interpolować zmienne z innymi typami danych, na przykład liczbami czy tablicami:

```PowerShell
$liczba = 21
"Dzisiaj jest $liczba dzień października" # wynik: Dzisiaj jest 21 dzień października
$kolory = @("niebieski", "zielony", "czerwony")
"Mam na sobie koszulkę w kolorze $($kolory[0])" # wynik: Mam na sobie koszulkę w kolorze niebieski
```

## Głębszy zanurzenie

Interpolacja stringów to bardzo popularna operacja w wielu językach programowania, a jej historia sięga lat 70. Przez lata powstało wiele różnych metod interpolacji, takich jak konkatenacja, formatowanie printf czy użycie znaków specjalnych, lecz w PowerShell interpolacja jest wygodniejsza i bardziej czytelna dzięki wykorzystaniu znaku dolara. Jedną z alternatyw dla interpolacji w PowerShell jest użycie operatora join, który pozwala na łączenie elementów tablicy z danym znacznikiem.

Jeśli chodzi o implementację, PowerShell wykorzystuje mechanizm nazywany "Subexpression syntax", dzięki któremu możemy łączyć wyrażenia z innymi wyrażeniami, zmiennymi czy funkcjami. Jest to bardzo użyteczne i pozwala na tworzenie bardziej rozbudowanych i elastycznych skryptów.

## Zobacz też

Jeśli chcecie dowiedzieć się więcej o interpolacji stringów w PowerShell, zapraszam do zapoznania się z [oficjalną dokumentacją Microsoft](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_quoting_rules). Koniecznie także wypróbujcie różne kombinacje i zastosowania interpolacji w swoich skryptach PowerShell.