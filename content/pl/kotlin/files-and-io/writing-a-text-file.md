---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:22.003528-07:00
description: "Jak to zrobi\u0107: Kotlin oferuje prost\u0105 metod\u0119 zapisywania\
  \ do plik\xF3w, wykorzystuj\u0105c standardow\u0105 bibliotek\u0119 bez potrzeby\
  \ dodatkowych bibliotek stron trzecich.\u2026"
lastmod: '2024-03-13T22:44:35.383539-06:00'
model: gpt-4-0125-preview
summary: "Kotlin oferuje prost\u0105 metod\u0119 zapisywania do plik\xF3w, wykorzystuj\u0105\
  c standardow\u0105 bibliotek\u0119 bez potrzeby dodatkowych bibliotek stron trzecich."
title: Pisanie pliku tekstowego
weight: 24
---

## Jak to zrobić:
Kotlin oferuje prostą metodę zapisywania do plików, wykorzystując standardową bibliotekę bez potrzeby dodatkowych bibliotek stron trzecich. Oto prosty przykład:

```kotlin
import java.io.File

fun main() {
    val textDoZapisania = "Witaj, zapisywanie pliku Kotlin!"
    File("przyklad.txt").writeText(textDoZapisania)
}
```
Ten fragment kodu tworzy plik o nazwie "przyklad.txt" w katalogu głównym projektu i zapisuje do niego ciąg `Witaj, zapisywanie pliku Kotlin!`. Jeśli plik już istnieje, zostanie nadpisany.

Dla bardziej kontrolowanego dopisywania do pliku lub zapisywania większych ilości danych, możesz użyć `appendText` lub `bufferedWriter()`:

```kotlin
import java.io.File

fun appendToFile() {
    val wiecejTekstu = "Dopisuję więcej tekstu."
    File("przyklad.txt").appendText(wiecejTekstu)
}

fun writeWithBufferedWriter() {
    val duzoTekstu = "Duże ilości tekstu...\nNa wielu liniach."
    File("wyjscie.txt").bufferedWriter().use { out ->
        out.write(duzoTekstu)
    }
}

fun main() {
    appendToFile() // Dopisuje tekst do istniejącego pliku
    writeWithBufferedWriter() // Zapisuje duże ilości danych tekstowych efektywnie
}
```

W funkcji `appendToFile` dodajemy więcej tekstu do "przyklad.txt" bez nadpisywania jego bieżącej zawartości. Funkcja `writeWithBufferedWriter` prezentuje efektywny sposób na zapisywanie dużych ilości tekstu lub danych, szczególnie przydatny w celu minimalizacji operacji I/O przy pracy z wieloma liniami lub dużymi plikami.

Te przykłady obejmują podstawowe operacje zapisu plików tekstowych w Kotlinie, prezentując prostotę i moc standardowej biblioteki Kotlin do operacji I/O na plikach.
