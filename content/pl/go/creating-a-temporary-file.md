---
title:                "Go: Tworzenie pliku tymczasowego"
simple_title:         "Tworzenie pliku tymczasowego"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Tworzenie pliku tymczasowego jest niezbędną częścią wielu programów w języku Go. Plik tymczasowy jest krótkotrwałym plikiem, którego zadaniem jest przechowywanie danych tymczasowych, takich jak przez chwilę potrzebne pliki lub podczas przetwarzania danych. Jest to wygodny sposób na przechowywanie i dostęp do danych w programie.

## Jak to zrobić

Tworzenie pliku tymczasowego w Go jest bardzo proste. Wystarczy użyć funkcji "ioutil.TempFile", która pozwala na utworzenie pliku tymczasowego w wybranym katalogu. Poniżej znajduje się przykładowy kod, który utworzy plik tymczasowy w katalogu "/tmp" i wyświetli jego nazwę:

```Go
file, err := ioutil.TempFile("/tmp", "example") // tworzenie pliku tymczasowego
if err != nil {
    log.Fatal(err)
}

defer os.Remove(file.Name()) // usunięcie pliku po zakończeniu

fmt.Println("Nazwa pliku tymczasowego:", file.Name()) // wyświetlenie nazwy pliku
```

Po uruchomieniu tego kodu, zobaczymy następujący wynik:

```Go
Nazwa pliku tymczasowego: /tmp/example923428401
```

Plik tymczasowy zostanie automatycznie usunięty po zakończeniu programu. Jest to ważne, aby nie pozostawiać niepotrzebnych plików tymczasowych w systemie.

## Głębsze zagłębienie

Powyższy kod jest tylko przykładem prostego tworzenia pliku tymczasowego w Go. Jest wiele innych funkcji i opcji, które można wykorzystać przy tworzeniu i zarządzaniu plikami tymczasowymi. Na przykład, można ustawić prefiks i sufiks dla nazwy pliku, określić określone atrybuty pliku, a także kopiować zawartość innego pliku do pliku tymczasowego. Szczegółowe informacje na temat wszystkich dostępnych funkcji można znaleźć w dokumentacji języka Go.

## Zobacz także

- Dokumentacja języka Go na temat tworzenia plików tymczasowych: https://golang.org/pkg/io/ioutil/#TempFile
- Przykładowe kompendium zawierające wiele przydatnych informacji na temat plików tymczasowych w Go: https://blog.tempfile.io/go-temporary-file-guide