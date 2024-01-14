---
title:                "Go: Tworzenie pliku tekstowego"
simple_title:         "Tworzenie pliku tekstowego"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/writing-a-text-file.md"
---

{{< edit_this_page >}}

### Dlaczego

Pisanie pliku tekstowego jest nieodłączną częścią programowania w języku Go. Jest to łatwy i szybki sposób na zapisywanie danych w zrozumiałej dla człowieka formie.

### Jak to zrobić

W języku Go istnieje wiele sposobów na zapisanie tekstu do pliku. Jednym z najprostszych jest użycie funkcji `WriteFile` z pakietu `io/ioutil`. Oto przykładowy kod:

```
paczka glówna

import (
    "fmt"
    "io/ioutil"
)

func main() {
    tekst := "To jest przykładowy tekst, który zostanie zapisany do pliku."
    err := ioutil.WriteFile("tekst.txt", []byte(tekst), 0644)
    if err != nil {
        fmt.Println("Błąd przy zapisywaniu pliku:", err)
        return
    }
    
    fmt.Println("Plik został zapisany.")
}
```

Powyższy kod wykorzystuje funkcję `WriteFile` do zapisania tekstu do pliku o nazwie "tekst.txt". Kod 0644 oznacza, że plik będzie miał uprawnienia do odczytu i zapisu dla właściciela, a tylko do odczytu dla innych użytkowników.

### W głębi

Aby zapisać dane w formacie innych niż tekst, można użyć funkcji `Write` z pakietu `os`. Ta funkcja przyjmuje jako parametry plik, do którego chcemy zapisać dane oraz slice bajtów reprezentujących dane. Oto przykładowy kod:

```
paczka główna

import (
    "fmt"
    "os"
)

func main() {
    plik, err := os.Create("dane.bin")
    if err != nil {
        fmt.Println("Błąd przy tworzeniu pliku:", err)
        return
    }
    defer plik.Close()
    
    dane := []byte{0x48, 0x65, 0x6C, 0x6C, 0x6F, 0x20, 0x57, 0x6F, 0x72, 0x6C, 0x64}
    _, err = plik.Write(dane)
    if err != nil {
        fmt.Println("Błąd przy zapisywaniu danych:", err)
        return
    }
    
    fmt.Println("Dane zostały zapisane.")
}
```

Ten kod tworzy plik "dane.bin" i zapisuje w nim dane w postaci slice'a bajtów. Funkcja `Write` zwraca również ilość zapisanych bajtów, która w tym przypadku jest pomijana przy użyciu `_`.

### Zobacz również

- Dokumentacja pakietu `io/ioutil`: https://golang.org/pkg/io/ioutil/
- Dokumentacja pakietu `os`: https://golang.org/pkg/os/
- Przykładowe zadania związane z pisanie plików w języku Go: https://gobyexample.com/writing-files