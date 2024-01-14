---
title:                "Kotlin: Tworzenie pliku tekstowego"
simple_title:         "Tworzenie pliku tekstowego"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie pliku tekstowego jest nieodłączną częścią pisania programów w Kotlinie. Jest to ważny proces, ponieważ pozwala na zapisywanie danych w formie tekstowej, co czyni je czytelnymi dla użytkownika i innych programów.

## Jak to zrobić

Pisanie pliku tekstowego jest bardzo proste w języku Kotlin. Wystarczy użyć funkcji `writeText()` i podać jako argument nazwę pliku oraz zawartość, którą chcemy zapisać. Następnie, za pomocą funkcji `readText()` możemy odczytać zapisane dane.

```Kotlin
// Przykładowa treść do zapisania
val text = "Cześć wszystkim czytającym ten plik!"

// Zapis do pliku
val file = File("example.txt")
file.writeText(text)

// Odczyt zapisanych danych
val result = file.readText()
println(result)
```

W powyższym przykładzie wprowadziliśmy zmienną `file`, która jest naszym plikiem tekstowym, oraz zmienną `text`, która zawiera przykładową treść do zapisania. Następnie, za pomocą funkcji `writeText()`, zapisaliśmy treść w pliku `example.txt`. Aby odczytać zapisane dane, użyliśmy funkcji `readText()` i przypisaliśmy je do zmiennej `result`.

Po uruchomieniu powyższego kodu, otrzymamy na wyjściu napis "Cześć wszystkim czytającym ten plik!".

## Deep Dive

W języku Kotlin możemy także tworzyć i pisać do plików przy użyciu klas `FileWriter` i `BufferedWriter`. Klasa `FileWriter` służy do zapisywania znaków w pliku, a `BufferedWriter` wykorzystuje pamięć podręczną, co przyspiesza proces zapisywania.

Ponadto, możemy wykorzystać operator `+` do dodawania tekstu w istniejącym pliku:

```Kotlin
val file = File("example.txt")
file.appendText(" Cześć jeszcze raz!")
```

Możemy także tworzyć nowe linie w pliku za pomocą funkcji `println()`:

```Kotlin
val file = File("example.txt")
file.appendText("Hej!")
file.println()
file.appendText("Cześć!")
```

## Zobacz też

- Dokumentacja języka Kotlin: https://kotlinlang.org/docs/home.html
- Poradnik "Jak czytać i pisać pliki w Kotlinie": https://kotlinexpertise.com/kotlin-read-write-file/
- Oficjalne repozytorium języka Kotlin na GitHubie: https://github.com/JetBrains/kotlin