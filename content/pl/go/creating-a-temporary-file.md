---
title:    "Go: Tworzenie tymczasowego pliku"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Dlaczego

Tworzenie tymczasowego pliku jest ważną częścią procesu programowania w Go. Umożliwia ono tworzenie i zapisywanie danych tymczasowych, które są potrzebne w trakcie wykonywania programu. Mogą to być na przykład dane tymczasowe, które są potrzebne do przechowywania wyników pośrednich lub do tymczasowego przechowywania danych pobranych ze zdalnego serwera. Tworzenie tymczasowego pliku jest również przydatne w celu zabezpieczenia danych przed przypadkowym nadpisaniem.

## Jak to zrobić

Tworzenie tymczasowego pliku w Go jest bardzo proste, wystarczy użyć funkcji `ioutil.TempFile ()`. Przyjmuje ona dwa argumenty, pierwszy to ścieżka, w której chcemy utworzyć plik tymczasowy, a drugi to nazwa pliku. Jeśli ścieżka jest pusta, plik zostanie utworzony w systemowym katalogu tymczasowym. Poniżej znajduje się przykładowy kod, który tworzy tymczasowy plik i wypisuje jego nazwę:

```Go
plik, err := ioutil.TempFile ("", "plik tymczasowy")
if err != nil {
    fmt.Println ("Błąd podczas tworzenia pliku:", err)
    return
}
defer plik.Close ()
fmt.Println ("Tymczasowy plik został utworzony:", plik.Name ())
```

Przykładowy output: `Tymczasowy plik został utworzony: C:\Users\Użytkownik\AppData\Local\Temp\plik tymczasowy123456`

## Zagłębienie w temat

Podczas tworzenia tymczasowego pliku, Go automatycznie generuje unikalną nazwę dla pliku, aby uniknąć konfliktów z istniejącymi plikami o tej samej nazwie. Ponadto, funkcja `ioutil.TempFile ()` zwraca obiekt typu `*os.File` i automatycznie go otwiera. Dzięki temu nie musimy martwić się o ręczne otwieranie pliku.

Możemy także określić własną, unikalną nazwę dla pliku, jeśli chcemy zachować kontrolę nad jego nazwą. W tym celu możemy wykorzystać funkcję `ioutil.TempDir ()` do utworzenia folderu tymczasowego, a następnie użyć tej samej funkcji `ioutil.TempFile ()`, ale podając nazwę folderu jako pierwszy argument. W ten sposób możemy mieć pełną kontrolę nad nazwą pliku.

## Zobacz także

- [Dokumentacja funkcji ioutil.TempFile](https://golang.org/pkg/io/ioutil/#TempFile)
- [Tutorial dotyczący tworzenia tymczasowych plików w Go](https://www.calhoun.io/creating-random-temp-files-in-go/)