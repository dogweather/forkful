---
title:                "Praca z plikami CSV"
aliases:
- /pl/swift/working-with-csv.md
date:                  2024-02-03T19:21:47.055452-07:00
model:                 gpt-4-0125-preview
simple_title:         "Praca z plikami CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

Praca z plikami CSV (wartości oddzielone przecinkami) polega na parsowaniu i generowaniu strukturalnych danych z plików tekstowych, gdzie każda linia reprezentuje rekord, a każdy rekord składa się z pól rozdzielonych przecinkami. Programiści często angażują się w tę czynność, aby łatwo importować, eksportować i manipulować danymi tabelarycznymi, korzystając z formatu, który jest szeroko obsługiwany na różnych platformach i językach programowania, ze względu na jego prostotę i czytelność dla człowieka.

## Jak to zrobić:

W języku Swift nie ma natywnej obsługi bezpośredniego parsowania plików CSV, ale możesz obsługiwać dane CSV, używając metod `String`, aby rozdzielić zawartość, lub korzystając z bibliotek stron trzecich, takich jak SwiftCSV, dla bardziej uproszczonego podejścia. Oto obie metody:

### Ręczne parsowanie bez zewnętrznych bibliotek
```swift
// Rozważ prosty ciąg CSV
let csvString = """
name,age,city
John Doe,29,New York
Jane Smith,34,Los Angeles
"""

// Podziel ciąg CSV na linie
let rows = csvString.components(separatedBy: "\n")

// Wyodrębnij klucze z pierwszego wiersza
let keys = rows.first?.components(separatedBy: ",")

// Iteruj po wierszach, zaczynając od drugiego
var result: [[String: String]] = []
for row in rows.dropFirst() {
    let values = row.components(separatedBy: ",")
    let dict = Dictionary(uniqueKeysWithValues: zip(keys!, values))
    result.append(dict)
}

// Przykładowe wyjście
print(result)
// Wyświetla: [{"city": "New York", "age": "29", "name": "John Doe"}, {"city": "Los Angeles", "age": "34", "name": "Jane Smith"}]
```
To podejście jest proste, ale niezbyt solidne, szczególnie w przypadku plików CSV zawierających specjalne przypadki, takie jak przecinki w wartościach, przerwy w liniach wewnątrz pól itp.

### Korzystanie z biblioteki SwiftCSV
Najpierw dodaj SwiftCSV do swojego projektu, włączając ją do zależności swojego `Package.swift`:
```swift
.package(url: "https://github.com/swiftcsv/SwiftCSV.git", from: "0.5.6")
```
Następnie zaimportuj ją i użyj w następujący sposób:
```swift
import SwiftCSV

// Zakładając, że `csvString` jest zdefiniowane jak wyżej

// Utwórz obiekt CSV
if let csv = try? CSV(string: csvString) {
    // Dostęp do wierszy jako słowników
    let rows = csv.namedRows
    
    // Przykładowe wyjście
    print(rows)
    // Wyświetla: [{"city": "New York", "age": "29", "name": "John Doe"}, {"city": "Los Angeles", "age": "34", "name": "Jane Smith"}]
}
```
SwiftCSV upraszcza parsowanie, automatycznie radząc sobie z niuansami takimi jak ujęte przecinki, przerwy w linii w polach i kodowanie znaków. Jednak pamiętaj, aby obsługiwać możliwe błędy w aplikacjach świata rzeczywistego, zwłaszcza przy pracy z zewnętrznymi źródłami danych.
