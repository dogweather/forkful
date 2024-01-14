---
title:    "Go: Pisanie do standardowego błędu"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Dlaczego?

Pisanie do standardowego błędu jest nieodłączną częścią procesu programowania w języku Go. Jest to ważne narzędzie, które pozwala programiście na raportowanie błędów, ostrzeżeń oraz innych wiadomości, które są krytyczne dla poprawnego działania aplikacji. W tym artykule dowiesz się, dlaczego warto pisać do standardowego błędu oraz jak to zrobić w prosty sposób.

## Jak to zrobić?

Kodowanie w Go jest stosunkowo proste, a pisanie do standardowego błędu jest jedną z podstawowych funkcji dostępnych w języku. Poniżej znajduje się przykład kodu wraz z komentarzami, który pokazuje, jak można użyć standardowego błędu w swoim kodzie:

```Go
package main

import (
  "fmt"
  "os"
)

func main() {
  fmt.Fprintf(os.Stderr, "To jest wiadomość błędu\n") // wypisuje wiadomość do standardowego błędu
}
```

Powyższy przykład używa funkcji `Fprintf` z pakietu `fmt`, aby wypisać wiadomość do standardowego błędu. Aby tego użyć, musimy najpierw zaimportować pakiet `os`.

Zobaczmy teraz, jak powyższy kod wygląda w konsoli:

```
$ go run main.go
To jest wiadomość błędu
```

Jak widzisz, wiadomość została wypisana do standardowego błędu w konsoli.

## Deep Dive

Pisanie do standardowego błędu jest szczególnie przydatne, gdy chcemy poinformować użytkownika o wystąpieniu błędu lub ostrzeżeniu. Jednak istnieje również możliwość przekierowania standardowego błędu do innych miejsc, takich jak pliki lub połączenie sieciowe.

Aby przekierować standardowy błędu do pliku, można użyć funkcji `SetOutput` z pakietu `os`:

```Go
package main

import (
  "fmt"
  "os"
)

func main() {
  outFile, err := os.OpenFile("błąd.log", os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0644)
  if err != nil {
    fmt.Fprintln(os.Stderr, "Nie można otworzyć pliku błąd.log:", err)
    os.Exit(1)
  }
  defer outFile.Close()

  fmt.Fprintln(os.Stderr, "Standardowy błąd został przekierowany do pliku błąd.log!")
  fmt.SetOutput(outFile)
  fmt.Fprintln(os.Stderr, "Ta wiadomość pojawi się w pliku błąd.log")
}
```

W powyższym przykładzie tworzymy nowy plik `błąd.log` i następnie przekierowujemy do niego standardowy błąd. Oczywiście, możesz użyć dowolnej nazwy pliku lub ścieżki do przekierowania błędu.

## Zobacz również

- Dokumentacja pakietu `fmt`: https://golang.org/pkg/fmt/
- Dokumentacja pakietu `os`: https://golang.org/pkg/os/
- Przekierowanie standardowego błędu do pliku: https://www.calhoun.io/how-to-get-rid-of-stdout-issues-in-golang/
- Tworzenie własnego loggera w Go: https://medium.com/@triktron/writing-a-logger-in-golang-d78248953287