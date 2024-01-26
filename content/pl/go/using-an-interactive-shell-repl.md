---
title:                "Korzystanie z interaktywnego shella (REPL)"
date:                  2024-01-26T04:14:48.493235-07:00
model:                 gpt-4-0125-preview
simple_title:         "Korzystanie z interaktywnego shella (REPL)"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
REPL (Read-Eval-Print Loop), czyli pętla czytaj-wykonaj-wypisz, umożliwia interakcję z kodem na żywo; czyta dane wejściowe, ocenia je, wypisuje wynik i rozpoczyna od nowa. Programiści używają go do testowania fragmentów kodu, debugowania oraz nauki nowych języków w czasie rzeczywistym.

## Jak to zrobić:
Go nie zawiera wbudowanego REPL, ale można użyć narzędzi stron trzecich. Jednym z popularnych narzędzi jest `gore`:

```go
// Zainstaluj gore używając
$ go install github.com/motemen/gore/cmd/gore@latest

// Uruchom gore
$ gore
gore version 0.5.0  :help dla pomocy
gore> :import fmt
gore> fmt.Println("Hello, Go REPL!")
Hello, Go REPL!
nil
```

## Szczegółowe omówienie
Początkowo rozwinięte dla Lispa, REPL-e są powszechne w dynamicznych językach takich jak Python czy Ruby. Go, będąc językiem statycznie typowanym, nie zawiera go na wstępie. Alternatywy dla `gore` obejmują `go-pry` i `yaegi`. Te narzędzia interpretują kod Go, pozwalając szybko eksplorować i weryfikować pomysły bez kompilowania pełnoprawnej aplikacji. Są one szczególnie przydatne dla początkujących oraz w kontekstach edukacyjnych, gdzie nacisk kładziony jest na naukę i eksperymentowanie.

## Zobacz także
- `gore`: [https://github.com/motemen/gore](https://github.com/motemen/gore)
- `go-pry`: [https://github.com/d4l3k/go-pry](https://github.com/d4l3k/go-pry) 
- `yaegi`: [https://github.com/traefik/yaegi](https://github.com/traefik/yaegi)