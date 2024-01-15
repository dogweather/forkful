---
title:                "Tworzenie pliku tekstowego"
html_title:           "Kotlin: Tworzenie pliku tekstowego"
simple_title:         "Tworzenie pliku tekstowego"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie pliku tekstowego może wydawać się proste i mało istotne, ale jest to niezwykle ważna umiejętność w programowaniu. Pliki tekstowe są powszechnie używane w wielu aplikacjach, na przykład do przechowywania ustawień, danych użytkownika czy logów. W tym artykule dowiesz się, jak w łatwy sposób napisać plik tekstowy w języku Kotlin.

## Jak to zrobić

Aby napisać plik tekstowy w Kotlinie, potrzebujemy dwóch rzeczy: ścieżki do pliku, w którym chcemy zapisać dane oraz samych danych, które chcemy zapisać. Kod będzie wyglądał mniej więcej tak:

```kotlin
val filePath = "/sciezka/do/pliku/tekstowego.txt"
val data = "To jest przykład tekstu, który chcemy zapisać do pliku."
```

Następnie wykorzystujemy klasę `File` z pakietu `java.io` do utworzenia obiektu reprezentującego nasz plik. W konstruktorze podajemy ścieżkę do naszego pliku:

```kotlin
val file = File(filePath)
```

Teraz wykorzystujemy obiekt `BufferedWriter` do zapisu danych do pliku. Służy on do buforowania danych, co pozwala na szybsze i wydajniejsze zapisywanie dużych ilości tekstu. Kod będzie wyglądał następująco:

```kotlin
val writer = BufferedWriter(FileWriter(file))
writer.write(data)
writer.close()
```

W powyższym kodzie tworzymy obiekt `BufferedWriter`, który przyjmuje jako argument obiekt `FileWriter`, który z kolei przyjmuje jako argument nasz obiekt `file` reprezentujący plik. Potem wywołujemy metodę `write` i przekazujemy jej nasze dane, a na końcu wywołujemy metodę `close`, aby zakończyć zapis do pliku.

## Deep Dive

Jeśli chcesz poznać więcej sposobów na zapisywanie danych do pliku, możesz zapoznać się z różnymi interfejsami z pakietu `java.io`, takimi jak `Writer`, `DataOutputStream` czy `PrintWriter`. Każdy z nich oferuje inne metody i możliwości, na przykład zapisywanie danych w różnych formatach czy też bardziej zaawansowane manipulacje plikiem.

Ważną rzeczą, którą warto pamiętać podczas pisania pliku tekstowego, jest obsługa wyjątków. Pliki mogą być niestabilne i niezawsze będzie możliwe ich zapisanie. Dlatego należy odpowiednio obsłużyć sytuację, gdy zapis do pliku się nie powiedzie. W tym wypadku warto skorzystać z bloku `try-catch` lub rzucić wyjątek dalej, aby inny fragment kodu mógł go obsłużyć.

## Zobacz też

- Dokumentacja pakietu `java.io` w języku Kotlin: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/index.html
- Przykładowe projekty z wykorzystaniem plików tekstowych w języku Kotlin na platformie GitHub: https://github.com/search?q=kotlin+file&type=Repositories