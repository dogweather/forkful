---
title:                "Go: Tworzenie pliku tymczasowego"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego
Dlaczego stworzenie tymczasowego pliku jest przydatne? Czasami podczas tworzenia aplikacji lub kompilacji kodu, wymagane jest tymczasowe przechowywanie danych lub plików. Stworzenie tymczasowego pliku pozwala na tymczasowe przechowywanie danych w pamięci i szybkie usunięcie go po zakończeniu procesu.

## Jak To Zrobić
Tworzenie tymczasowego pliku w języku Go jest bardzo łatwe. Wystarczy użyć funkcji `ioutil.TempFile()` i podać mu ścieżkę i prefiks nazwy pliku. Oto przykładowy kod:

```Go
func main() {
    // Utworznie tymczasowego pliku w katalogu systemowym
    tempFile, err := ioutil.TempFile("", "example")
    if err != nil {
        log.Fatal(err)
    }

    // Wyświetlenie ścieżki i nazwy tymczasowego pliku
    fmt.Println("Tymczasowy plik utworzony w:", tempFile.Name())

    // Zapisanie zawartości do tymczasowego pliku
    defer tempFile.Close()
    text := []byte("Przykładowa zawartość tymczasowego pliku")
    if _, err := tempFile.Write(text); err != nil {
        log.Fatal(err)
    }

    // Odczytanie zawartości z tymczasowego pliku
    tempFileData, err := ioutil.ReadFile(tempFile.Name())
    if err != nil {
        log.Fatal(err)
    }
    fmt.Printf("Zawartość tymczasowego pliku: %s", tempFileData)
}
```

Przykładowy output:
```
Tymczasowy plik utworzony w: /tmp/example292086630
Zawartość tymczasowego pliku: Przykładowa zawartość tymczasowego pliku
```

## Głębszy Przegląd
Funkcja `ioutil.TempFile()` tworzy tymczasowy plik w katalogu, który jest podany jako pierwszy argument. Jeśli zostanie podany pusty string, plik zostanie utworzony w katalogu systemowym dla tymczasowych plików. Drugim argumentem jest prefiks nazwy pliku, co oznacza, że nazwa tymczasowego pliku będzie zaczynać się od podanego prefiksu, ale będzie również zawierać losowo wygenerowany ciąg znaków, aby uniknąć konfliktów z innymi plikami. 

W przypadku, gdy nie jest wymagane przechowywanie pliku w pamięci, lepszym rozwiązaniem jest użycie funkcji `ioutil.TempDir()` do utworzenia tymczasowego katalogu. Jest to szczególnie przydatne, jeśli aplikacja wymaga przechowywania wielu tymczasowych plików. 

## Zobacz też
- Dokumentacja języka Go dotycząca funkcji `ioutil.TempFile()`: https://golang.org/pkg/io/ioutil/#TempFile
- Listowanie zawartości katalogu i usuwanie zawartości tymczasowego katalogu: https://golang.org/pkg/io/ioutil/#TempDir
- Obsługa błędów przy użyciu funkcji `log.Fatal()`: https://golang.org/pkg/log/#Fatal