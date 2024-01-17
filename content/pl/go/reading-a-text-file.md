---
title:                "Odczytywanie pliku tekstowego"
html_title:           "Go: Odczytywanie pliku tekstowego"
simple_title:         "Odczytywanie pliku tekstowego"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Czytanie pliku tekstowego to jedna z podstawowych czynności programistycznych w języku Go. Polega ona na odczytaniu zawartości pliku tekstowego i wykorzystaniu jej w celu wykonania określonych operacji. Programiści często korzystają z tej funkcji, ponieważ umożliwia ona na łatwe i szybkie pobranie danych z zewnętrznego źródła.

## Jak to zrobić:
Go oferuje kilka sposobów na odczytanie pliku tekstowego. Jednym z nich jest użycie funkcji `ReadFile` z biblioteki `io/ioutil`. Przykładowy kod wykorzystujący tę funkcję może wyglądać następująco:

```Go 
file, err := ioutil.ReadFile("nazwa_pliku.txt")
if err != nil {
    panic(err)
}
fmt.Print(string(file))
```

Powyższy kod otwiera plik o nazwie "nazwa_pliku.txt" i odczytuje jego zawartość za pomocą funkcji `ReadFile`. Następnie za pomocą funkcji `string` konwertuje zawartość na postać tekstową i wypisuje ją na ekran. 

## Głębszy Zanurzenie:
Czytanie plików tekstowych jest częścią programowania od samego początku. W języku Go można również użyć funkcji `Open` z biblioteki `os` lub `bufio` do odczytania pliku wiersz po wierszu. Alternatywnie, można również użyć gotowej biblioteki `FileScanner`. Podczas odczytu pliku ważne jest użycie odpowiedniego kodowania znaków, w przeciwnym razie odczytane dane mogą być niepoprawne. 

## Zobacz również:
Możesz przeczytać więcej na temat czytania plików tekstowych w języku Go tutaj:
- [Oficjalna dokumentacja Go](https://golang.org/pkg/io/ioutil/)
- [GitHub Go tutorial](https://github.com/golang/go/wiki/Io)
- [Blog o Go](https://blog.golang.org/io2011)