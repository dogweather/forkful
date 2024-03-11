---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:44.647749-07:00
description: "U\u017Cywanie debugera w programowaniu w Go polega na wykorzystaniu\
  \ narz\u0119dzi lub funkcji do inspekcji i modyfikacji stanu dzia\u0142aj\u0105\
  cego programu w celu\u2026"
lastmod: '2024-03-11T00:14:08.028870-06:00'
model: gpt-4-0125-preview
summary: "U\u017Cywanie debugera w programowaniu w Go polega na wykorzystaniu narz\u0119\
  dzi lub funkcji do inspekcji i modyfikacji stanu dzia\u0142aj\u0105cego programu\
  \ w celu\u2026"
title: Korzystanie z debugera
---

{{< edit_this_page >}}

## Co i Dlaczego?

Używanie debugera w programowaniu w Go polega na wykorzystaniu narzędzi lub funkcji do inspekcji i modyfikacji stanu działającego programu w celu zrozumienia jego zachowań lub diagnozowania problemów. Programiści robią to, aby efektywnie znajdować i naprawiać błędy, optymalizować wydajność oraz zapewniać poprawność swojego kodu.

## Jak to zrobić:

Go oferuje wbudowaną funkcjonalność do debugowania o nazwie `delve`. Jest to pełnoprawne narzędzie debugujące, które umożliwia wykonanie programów w Go krok po kroku, inspekcję zmiennych programu oraz ocenę wyrażeń.

Aby zacząć, musisz najpierw zainstalować `delve`. Możesz to zrobić, wykonując:

```shell
go get -u github.com/go-delve/delve/cmd/dlv
```

Teraz, debugujmy prosty program w Go. Załóżmy program `main.go`:

```go
package main

import "fmt"

func main() {
    wiadomosc := "Debugowanie w Go"
    fmt.Println(wiadomosc)
}
```

Aby rozpocząć debugowanie tego programu, otwórz terminal w katalogu projektu i wykonaj:

```shell
dlv debug
```

To polecenie kompiluje program z wyłączonymi optymalizacjami (aby poprawić doświadczenie z debugowaniem), uruchamia go i dołącza do niego debugger.

Gdy `delve` jest uruchomione, znajdujesz się w interaktywnej powłoce debugera. Oto kilka podstawowych poleceń:

- `break main.main` ustawia punkt przerwania w funkcji `main`.
- `continue` wznawia wykonanie programu do momentu trafienia na punkt przerwania.
- `print wiadomosc` wydrukuje wartość zmiennej `wiadomosc`.
- `next` przechodzi do wykonania następnej linii programu.
- `quit` wychodzi z debugera.

Wyjście, gdy trafiamy na punkt przerwania i drukujemy zmienną, może wyglądać tak:

```shell
Breakpoint 1 at 0x49ecf3 for main.main() ./main.go:6
> main.main() ./main.go:6 (hits goroutine(1):1 total:1) (PC: 0x49ecf3)
     1: package main
     2:
     3: import "fmt"
     4:
     5: func main() {
     6: =>    wiadomosc := "Debugowanie w Go"
     7:       fmt.Println(wiadomosc)
     8: }
(dlv) print wiadomosc
"Debugowanie w Go"
```

Za pomocą tych poleceń możesz krok po kroku przeglądać swój program, inspekcjonując stan w miarę postępu, aby zrozumieć, jak się zachowuje i identyfikować ewentualne problemy.

## Dogłębna analiza

Wybór `delve` jako narzędzia do debugowania w Go zamiast tradycyjnych narzędzi takich jak GDB (GNU Debugger) wynika głównie z natury modelu wykonania i środowiska uruchomieniowego Go. GDB nie został początkowo zaprojektowany z myślą o środowisku uruchomieniowym Go, co sprawia, że `delve` jest bardziej odpowiednim wyborem dla deweloperów w Go. `Delve` jest specjalnie zaprojektowany dla Go, oferując bardziej intuicyjne doświadczenie debugowania dla gorutyn, kanałów i innych konstrukcji specyficznych dla Go.

Ponadto, `delve` obsługuje szeroki zakres funkcji, wykraczających poza te oferowane przez podstawowy GDB podczas pracy z programami w Go. Obejmują one, między innymi: dołączanie do działających procesów w celu debugowania; warunkowe punkty przerwania; oraz ocenianie skomplikowanych wyrażeń, które mogą obejmować prymitywy współbieżności Go.

Chociaż `delve` jest preferowanym debugerem wielu deweloperów Go, warto zauważyć, że narzędziownia Go również zawiera lżejsze formy wsparcia debugowania, takie jak wbudowane narzędzie `pprof` do profilowania i narzędzie `trace` do wizualizacji współbieżności. Te narzędzia czasami mogą zapewnić szybszą lub bardziej ogólną ścieżkę do diagnozowania problemów z wydajnością programu lub błędów współbieżności, co może być uzupełnieniem lub nawet preferowaną metodą w zależności od kontekstu debugowania.
