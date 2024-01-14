---
title:    "Go: Sprawdzanie istnienia katalogu"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego 

Pisanie programów to nie tylko tworzenie rozwiązań, ale również dbanie o ich stabilność i niezawodność. Sprawdzenie istnienia katalogu jest ważnym elementem tego procesu, ponieważ pozwala uniknąć błędów i niepożądanych skutków.

## Jak to zrobić

W języku Go sprawdzenie, czy dany katalog istnieje, jest bardzo proste. Wystarczy użyć funkcji `os.Stat(path string)` i sprawdzić, czy zwracany błąd jest równy `nil`. Poniżej znajduje się przykładowy kod oraz jego wynik:

```Go
package main

import(
    "fmt"
    "os"
)

func main() {
    path := "mydir" // zmienna z nazwą katalogu
    
    if _, err := os.Stat(path); err == nil {
        fmt.Printf("Katalog %s istnieje\n", path)
    } else {
        fmt.Printf("Katalog %s nie istnieje\n", path)
    }
}
```

Wynik:

```
Katalog mydir istnieje
```

W powyższym przykładzie użyto deklaracji `if` z warunkiem oraz funkcji `fmt.Printf()` do wyświetlenia komunikatu.

## Głębsze zanurzenie

Powyższa metoda sprawdzania istnienia katalogu jest bardzo prosta i działa w większości przypadków. Jednakże w niektórych sytuacjach może nie być wystarczająca, na przykład jeśli katalog jest ukryty lub nie ma uprawnień do jego odczytania. W takich przypadkach lepszym rozwiązaniem może być użycie funkcji `os.FileInfo.IsDir()` w celu sprawdzenia, czy dana ścieżka jest katalogiem. Poniżej znajduje się przykładowy kod:

```Go
package main

import(
    "fmt"
    "os"
)

func main() {
    path := "mydir" // zmienna z nazwą katalogu
    
    fileInfo, err := os.Stat(path)
    if err != nil {
        fmt.Printf("Nie udało się uzyskać informacji o pliku %s\n", path)
        return
    }
    
    if fileInfo.IsDir() {
        fmt.Printf("%s jest katalogiem\n", path)
    } else {
        fmt.Printf("%s nie jest katalogiem\n", path)
    }
}
```

Wynik:

```
mydir jest katalogiem
```

Zauważ, że w tym przypadku najpierw pobieramy informacje o pliku przy użyciu funkcji `os.Stat()`, a następnie sprawdzamy, czy jest to katalog przy użyciu metody `IsDir()` obiektu `fileInfo`.

## Zobacz także

- [Dokumentacja języka Go](https://golang.org/doc/)
- [Oficjalna strona Go](https://golang.org/)
- [Blog Go dla świeżo upieczonych programistów](https://pl.golang.org/)