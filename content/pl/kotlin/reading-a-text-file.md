---
title:                "Kotlin: Czytanie pliku tekstowego"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Czytanie plików tekstowych jest częstym zadaniem w programowaniu. Jest to niezbędne, gdy chcemy przetworzyć duże ilości informacji zapisanych w pliku lub gdy musimy wczytywać dane w określonym formacie. W tym artykule dowiesz się, jak w łatwy sposób przeczytać plik tekstowy w języku Kotlin.

## Jak to zrobić

Do wczytania pliku tekstowego w Kotlinie wykorzystamy funkcję `readText()` i podamy jako argument ścieżkę do pliku, który chcemy przeczytać. Na przykład:

```Kotlin
val tekst = File("moj_plik.txt").readText()
println(tekst)
```

Powyższy kod otwiera plik tekstowy o nazwie "moj_plik.txt" i wczytuje jego zawartość do zmiennej tekstowej. Następnie wypisuje ten tekst w konsoli. 

Możemy również wykorzystać pętlę `forEachLine` do wczytywania plików tekstowych wiersz po wierszu. W tym przypadku musimy użyć obiektu `BufferedReader`, który umożliwi nam wczytywanie wierszy z pliku. Przykładowy kod może wyglądać tak:

```Kotlin
val reader = BufferedReader(FileReader("moj_plik.txt"))
reader.forEachLine { wiersz ->
    println(wiersz)
}
```

## Dogłębna analiza

W przypadku wczytywania plików tekstowych w Kotlinie musimy uważać na obsługę błędów. Warto dodać obsługę wyjątków, na przykład gdy plik, który chcemy wczytać nie istnieje lub występują problemy z dostępem do niego. Możemy wykorzystać konstrukcję try-catch, aby obsłużyć ewentualne błędy.

Pamiętaj również, aby w odpowiedniej chwili zamknąć obiekty `File` i `BufferedReader` przy pomocy metody `close()`.

## Zobacz również 

- [Dokumentacja Kotlina na temat operacji na plikach](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/-file/)
- [Poradnik na YouTube o wczytywaniu plików tekstowych w Kotlinie](https://www.youtube.com/watch?v=7fsWZoVLAuY)
- [Przykłady kodu z wykorzystaniem operacji na plikach tekstowych w Kotlinie](https://gist.github.com/urbanrolewski/282eedf78d54d22514e996600c9f3213)