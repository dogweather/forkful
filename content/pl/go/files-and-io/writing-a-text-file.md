---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:15:01.365411-07:00
description: "Jak to zrobi\u0107: W Go do zapisywania w pliku tekstowym s\u0142u\u017C\
  \u0105 pakiety `os` i `io/ioutil` (dla wersji Go <1.16) lub `os` i `io` razem z\
  \ pakietem `os` dla Go w\u2026"
lastmod: '2024-03-13T22:44:34.873444-06:00'
model: gpt-4-0125-preview
summary: "W Go do zapisywania w pliku tekstowym s\u0142u\u017C\u0105 pakiety `os`\
  \ i `io/ioutil` (dla wersji Go <1.16) lub `os` i `io` razem z pakietem `os` dla\
  \ Go w wersji 1.16 i nowszych, co demonstruje filozofi\u0119 Go dotycz\u0105c\u0105\
  \ prostoty i efektywno\u015Bci."
title: Pisanie pliku tekstowego
weight: 24
---

## Jak to zrobić:
W Go do zapisywania w pliku tekstowym służą pakiety `os` i `io/ioutil` (dla wersji Go <1.16) lub `os` i `io` razem z pakietem `os` dla Go w wersji 1.16 i nowszych, co demonstruje filozofię Go dotyczącą prostoty i efektywności. Nowe API promuje lepsze praktyki z prostszą obsługą błędów. Zanurkujmy w to, jak stworzyć i zapisać do pliku tekstowego używając pakietu `os` w Go.

Najpierw upewnij się, że Twoje środowisko Go jest ustawione i gotowe. Następnie utwórz plik `.go`, na przykład `writeText.go`, i otwórz go w swoim edytorze tekstów lub IDE.

Oto prosty przykład, który zapisuje ciąg do pliku o nazwie `example.txt`:

```go
package main

import (
    "os"
    "log"
)

func main() {
    content := []byte("Witajcie, czytelnicy Wired!\n")

    // Stwórz lub nadpisz plik example.txt
    err := os.WriteFile("example.txt", content, 0644)
    if err != nil {
        log.Fatal(err)
    }
}
```

Kiedy uruchomisz ten kod za pomocą `go run writeText.go`, stworzy (lub nadpisze, jeśli już istnieje) plik o nazwie `example.txt` z zawartością "Witajcie, czytelnicy Wired!".

### Dołączanie do pliku
Co jeśli chcesz dołączyć treść? Go zapewnia elastyczny sposób na radzenie sobie z tym również:

```go
file, err := os.OpenFile("example.txt", os.O_APPEND|os.O_WRONLY|os.O_CREATE, 0644)
if err != nil {
    log.Fatal(err)
}
defer file.Close()

if _, err := file.WriteString("Dołączam więcej tekstu.\n"); err != nil {
    log.Fatal(err)
}
```

Ten fragment otwiera `example.txt` w trybie dołączania, zapisuje dodatkową linię i zapewnia prawidłowe zamknięcie pliku nawet jeśli wystąpi błąd.

## Głębsze spojrzenie
Ewolucja podejścia Go do obsługi plików odzwierciedla szersze zaangażowanie w prostotę i wydajność kodu. Wczesne wersje bardziej polegały na pakiecie `ioutil`, wymagając nieco większej rozwlekłości i nieznacznie wyższego potencjału dla błędów. Punkt zwrotny w kierunku zwiększenia funkcjonalności w pakietach `os` i `io`, szczególnie od wersji 1.16, ilustruje proaktywne kroki Go w kierunku usprawnienia operacji na plikach, zachęcając do bardziej spójnej obsługi błędów oraz czyniąc język bardziej przystępnym.

Choć wbudowane biblioteki Go są adekwatne dla wielu przypadków użycia, istnieją scenariusze, w których preferowane mogą być alternatywne pakiety lub zewnętrzne biblioteki, zwłaszcza przy bardziej złożonych operacjach na plikach lub podczas pracy w większych frameworkach, które dostarczają własne abstrakcje do obsługi plików. Jednakże, dla bezpośrednich, prostych zadań związanych z zapisywaniem plików, standardowa biblioteka często zapewnia najbardziej efektywną i idiomatyczną drogę naprzód w programowaniu w Go. Przejście w kierunku prostszych, bardziej zintegrowanych API do operacji na plikach nie tylko ułatwia pisanie i utrzymanie kodu w Go, ale również wzmacnia filozofię języka dotyczącą prostoty, czytelności i praktyczności.
