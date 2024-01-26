---
title:                "Korzystanie z debugera"
date:                  2024-01-26T03:49:13.976767-07:00
model:                 gpt-4-0125-preview
simple_title:         "Korzystanie z debugera"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/using-a-debugger.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Użycie debugera jest jak posiadanie GPS-u w dżungli kodu; prowadzi cię do źródła problemu. Programiści używają debuggerów, by krok po kroku przejść przez swój kod, inspekcjonować zmienne i zrozumieć przepływ, co ułatwia łapanie błędów i optymalizację wydajności.

## Jak to zrobić:
Go ma wbudowane narzędzie do debugowania o nazwie Delve (`dlv`). Aby zacząć, zainstaluj Delve, napisz prosty program w Go, a następnie uruchom go przez debugger.

```Go
// Najpierw zainstaluj Delve
// go get -u github.com/go-delve/delve/cmd/dlv

// Przykładowy program w Go, zapisz jako main.go
package main

import "fmt"

func main() {
    message := "Debugowanie z Delve!"
    fmt.Println(message)
}

// Uruchom swój program z Delve
// dlv debug

// Niektóre podstawowe komendy Delve:
// (dlv) break main.main // ustawia punkt przerwania w funkcji main
// (dlv) continue // uruchamia program do momentu napotkania punktu przerwania lub zakończenia programu
// (dlv) step // pojedynczy krok przez program
// (dlv) print message // wyświetla aktualną wartość zmiennej 'message'
// (dlv) quit // wyjście z Delve
```

Uruchomienie `dlv debug` rozpoczyna sesję debugowania. Kiedy trafisz na ustawiony punkt przerwania, możesz krok po kroku przejść przez swój program i zobaczyć, co dzieje się pod pokładem.

## Głębsze zanurzenie
Historycznie programiści Go używali kilku narzędzi do debugowania, takich jak GDB (GNU Debugger), jednak napotykali wyzwania, ponieważ GDB nie był dostosowany do środowiska wykonawczego Go i gorutyn. Delve przyszło z pomocą, oferując lepsze wsparcie dla unikalnych funkcji Go.

Istnieją alternatywy dla Delve, takie jak `go-dbg`, a nawet wbudowane wsparcie debugera w środowiskach IDE takich jak Visual Studio Code i GoLand, które opakowują Delve dla bardziej przyjaznego użytkownikowi doświadczenia.

Od strony implementacji, Delve działa używając pakietów `runtime` i `debug/gosym`, wśród innych, aby uzyskać dostęp do i interpretować symbole programu Go oraz informacje o czasie wykonania. Jest on nieustannie aktualizowany, aby nadążać za nowymi funkcjami języka i wersjami.

## Zobacz również
- Oficjalne Repozytorium Delve: https://github.com/go-delve/delve
- Samouczek Debugera Go od Zespołu Go: https://golang.org/doc/gdb
- Debugowanie Go w Visual Studio Code: https://code.visualstudio.com/docs/languages/go#_debugging