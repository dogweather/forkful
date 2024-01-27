---
title:                "Edycja plików w miejscu przy użyciu jednolinijkowców CLI"
date:                  2024-01-27T16:20:55.275241-07:00
model:                 gpt-4-0125-preview
simple_title:         "Edycja plików w miejscu przy użyciu jednolinijkowców CLI"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data and Text Processing"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/editing-files-in-place-with-cli-one-liners.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Edycja plików w miejscu za pomocą jednolinijkowych poleceń CLI w PowerShellu polega na dokonywaniu bezpośrednich modyfikacji plików z linii poleceń, bez potrzeby otwierania ich w edytorze. To podejście pozwala zaoszczędzić czas i może być szczególnie przydatne do przetwarzania wsadowego lub automatyzacji powtarzalnych zadań edycyjnych w wielu plikach.

## Jak to zrobić:

### Zastępowanie tekstu w pojedynczym pliku

Zacznijmy od prostego zadania: chcesz zastąpić wszystkie wystąpienia "oldtext" na "newtext" w pliku o nazwie example.txt. Oto jak to zrobisz:

```PowerShell
(Get-Content example.txt) -replace 'oldtext', 'newtext' | Set-Content example.txt
```

Ta jednolinijkowa komenda odczytuje zawartość, wykonuje zamianę i zapisuje zawartość z powrotem do oryginalnego pliku.

### Edycja wielu plików

Co jeśli potrzebujesz zastosować tę samą zmianę w wielu plikach? Oto podejście z użyciem pętli:

```PowerShell
Get-ChildItem *.txt | ForEach-Object {
  (Get-Content $_) -replace 'oldtext', 'newtext' | Set-Content $_
}
```

Ten fragment kodu znajduje wszystkie pliki `.txt` w bieżącym katalogu i zastępuje w każdym z nich "oldtext" na "newtext".

### Dodawanie zawartości na początku lub końcu plików

Dodawanie lub dopisywanie zawartości również może być zautomatyzowane:

```PowerShell
# Dopisywanie na początku
"New first line`n" + (Get-Content example.txt) | Set-Content example.txt

# Dopisywanie na końcu
(Get-Content example.txt) + "`nNew last line" | Set-Content example.txt
```

W tym przypadku po prostu konkatenujemy nową zawartość przed lub po istniejącej zawartości i zapisujemy ją z powrotem.

## Dogłębna analiza

Historycznie, edycja w miejscu jest bardziej kojarzona z narzędziami Unixowymi takimi jak `sed` i `awk`. PowerShell, jako stosunkowo nowy nabytek, nie zawiera wbudowanej funkcji edycji w miejscu jako takiej. Wynika to częściowo z jego filozofii projektowej, podkreślającej znaczenie obiektów nad strumieniami tekstowymi, w przeciwieństwie do narzędzi Unixowych, które traktują większość danych wejściowych jako tekst.

Alternatywy dla PowerShella do tego zadania obejmują korzystanie z tradycyjnych narzędzi Unixowych dostępnych na Windows przez Cygwin lub podsystem Windows dla systemu Linux (WSL). Te narzędzia często oferują bardziej zwięzłą składnię do edycji w miejscu ze względu na ich tekstocentryczny projekt.

Pod względem implementacji ważne jest zauważenie, że podejście PowerShella polega na odczytaniu całego pliku do pamięci, wprowadzeniu zmian, a następnie zapisaniu go z powrotem. Chociaż ta metoda sprawdza się dobrze dla plików o umiarkowanym rozmiarze, może stać się nieefektywna dla bardzo dużych plików. W takich przypadkach można rozważyć bezpośrednie użycie metod `.NET` lub sięgnięcie po alternatywne narzędzia zaprojektowane do przesyłania strumieniowego dużych ilości danych.

Pomimo tych rozważań, elastyczność PowerShella i obszerny zestaw funkcji czynią go niezastąpionym narzędziem do manipulowania plikami bezpośrednio z linii poleceń, szczególnie dla osób już zaznajomionych z ekosystemem Windows lub zarządzających środowiskami wieloplatformowymi.
