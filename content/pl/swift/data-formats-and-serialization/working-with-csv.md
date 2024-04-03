---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:47.055452-07:00
description: "Jak to zrobi\u0107: W j\u0119zyku Swift nie ma natywnej obs\u0142ugi\
  \ bezpo\u015Bredniego parsowania plik\xF3w CSV, ale mo\u017Cesz obs\u0142ugiwa\u0107\
  \ dane CSV, u\u017Cywaj\u0105c metod `String`, aby\u2026"
lastmod: '2024-03-13T22:44:35.776917-06:00'
model: gpt-4-0125-preview
summary: "W j\u0119zyku Swift nie ma natywnej obs\u0142ugi bezpo\u015Bredniego parsowania\
  \ plik\xF3w CSV, ale mo\u017Cesz obs\u0142ugiwa\u0107 dane CSV, u\u017Cywaj\u0105\
  c metod `String`, aby rozdzieli\u0107 zawarto\u015B\u0107, lub korzystaj\u0105c\
  \ z bibliotek stron trzecich, takich jak SwiftCSV, dla bardziej uproszczonego podej\u015B\
  cia."
title: Praca z plikami CSV
weight: 37
---

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
