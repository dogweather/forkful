---
title:    "Elm: Odczytywanie pliku tekstowego"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Dlaczego

Czy wiesz, że odczytywanie plików tekstowych jest jedną z podstawowych czynności w programowaniu? Bez tego umiejętności, trudno byłoby zautomatyzować proces tworzenia raportów, manipulować danymi lub wykonać wiele innych zadań. W tym artykule dowiesz się, jak odczytywać pliki tekstowe w języku Elm i jak może to ułatwić Twoje codzienne wyzwania.

## Jak to zrobić

Odczytywanie plików tekstowych w języku Elm jest bardzo proste i wymaga używania tylko jednej funkcji - `File.lines`. Aby ją wykorzystać, musisz mieć dostęp do pliku tekstowego, który chcesz odczytać. Zanim przejdziemy do kodu, warto zauważyć, że odczytywanie plików tekstowych odbywa się asynchronicznie, co oznacza, że musisz użyć funkcji `Task` do manipulowania odczytanymi danymi.

Oto przykładowy kod w języku Elm, który odczytuje plik tekstowy `example.txt` i wyświetla zawartość na ekranie:

```
import File exposing (lines)
import Task exposing (attempt)
import String

main =
    do
        -- użycie funkcji `attempt` do odczytania pliku
        fileContentsResult = attempt (\_ -> File.lines "example.txt") ()

        -- pobranie wyniku lub błędu odczytywania pliku
        case fileContentsResult of
            -- jeśli nie ma błędu, wyświetl zawartość pliku
            Ok fileContents ->
                -- `List.map` zostanie użyte do wyświetlenia zawartości
                -- pliku, która zostanie wypisana w konsoli
                List.map (String.fromList >> (Task.succeed >> Task.perform identity)) fileContents

            -- jeśli wystąpi błąd, wyświetl komunikat o błędzie
            Err err ->
                Task.perform identity (Task.succeed (Debug.crash (Debug.toString err)))
```

Kod ten wykorzystuje funkcję `File.lines` do odczytania zawartości pliku tekstowego i wyświetla ją na ekranie przy użyciu funkcji `List.map`.

Przykładowy plik `example.txt` może wyglądać tak:

```
Hello
World
```

Po wykonaniu powyższego kodu, zostanie wyświetlony tekst: `["Hello", "World"]` oznaczający, że zawartość pliku została poprawnie odczytana i wyświetlona.

## Głębsze zanurzenie

Choć przykładowy kod wydaje się prosty, warto zauważyć kilka ważnych rzeczy. Po pierwsze, funkcja `File.lines` nie zwraca natychmiast wyniku, ale funkcję `Task`, która musi zostać wykonana za pomocą funkcji `Task.perform`. Dodatkowo, jeśli wystąpi błąd podczas odczytywania pliku, będzie on zwrócony przy użyciu `Err`, dlatego ważne jest, aby obsłużyć taką sytuację w kodzie.

Kolejną ważną rzeczą do zapamiętania jest to, że funkcja `File.lines` nie obsługuje kodowania pliku. Jeśli Twój plik tekstowy jest zakodowany w innym formacie niż UTF-8, musisz użyć funkcji `File.read` i dopasować odpowiedni dekoder do pliku. 

Wreszcie, warto wspomnieć, że funkcja `File.lines` zwraca listę linii w pliku, co oznacza, że jeśli plik zawiera tylko jedną linię, wynik będzie wyglądał tak: `["Hello"]`.

## Zobacz również

- [Dokumentacja języka Elm (pliki tekstowe)](https://elm-lang.org/docs/file)
- [Kod źródłowy przyk