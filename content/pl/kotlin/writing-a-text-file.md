---
title:    "Kotlin: Tworzenie pliku tekstowego"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Dlaczego warto pisać pliki tekstowe? Pisanie plików tekstowych jest ważną częścią programowania, ponieważ jest to metoda przechowywania danych w formacie czytelnym dla ludzi. Dzięki temu możemy łatwoedytować, odczytywać i udostępniać informacje. Pliki tekstowe są bardzo wygodnym sposobem na przechowywanie prostych danych, takich jak listy, informacje kontaktowe, ustawienia czy nawet proste bazy danych. W tym artykule dowiesz się, jak napisać plik tekstowy za pomocą języka Kotlin.

## Jak to zrobić

```Kotlin
fun main() {
    val file = File("moj_plik.txt") // Tworzy obiekt File dla pliku "moj_plik.txt"
    file.writeText("To jest przykładowy tekst") // Zapisuje tekst do pliku
    val text = file.readText() // Odczytuje tekst z pliku
    println(text) // Wyświetla zawartość pliku
}
```

Output:
```
To jest przykładowy tekst
```

W powyższym przykładzie najpierw tworzymy obiekt File dla pliku, którego chcemy użyć. Następnie, za pomocą funkcji `writeText()` zapisujemy wybrany przez nas tekst do pliku. W kolejnej linijce odczytujemy ten tekst z pliku i wypisujemy go na ekranie. Proste, prawda?

Warto również pamiętać, że pliki tekstowe można również czytać i zapisywać w różnych kodowaniach, co jest szczególnie ważne przy pracach z językami, które używają znaków spoza standardowego kodowania ASCII.

## Głębsza analiza

Pliki tekstowe są zapisywane w formacie ASCII, co oznacza, że można w nich przechowywać tylko znaki z podstawowego zestawu znaków. Aby przechowywać inne znaki, takie jak polskie litery, znaki diakrytyczne czy znaki specjalne, należy użyć odpowiedniego kodowania, na przykład UTF-8. 

Istnieją różne metody zapisu i odczytu danych z plików tekstowych w języku Kotlin. Mogą to być między innymi funkcje `writeText()` i `readText()` jak pokazane powyżej, ale również inne metody, takie jak `bufferedWriter()` lub `inputStreamReader()`, które pozwalają na bardziej zaawansowane operacje na plikach.

Podczas pisania plików tekstowych warto również pamiętać o bezpieczeństwie danych. Wrażliwe informacje należy zawsze szyfrować, aby uniemożliwić dostęp nieautoryzowanym osobom.

## Zobacz również

- [Dokumentacja języka Kotlin](https://kotlinlang.org/docs/home.html)
- [Kurs programowania w Kotlinie na Courserze](https://www.coursera.org/learn/kotlin-for-java-developers)
- [Blog o języku Kotlin](https://blog.kotlin-academy.com/)