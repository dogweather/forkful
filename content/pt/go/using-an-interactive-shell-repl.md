---
title:                "Usando um shell interativo (REPL)"
date:                  2024-01-26T04:14:36.481468-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando um shell interativo (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Um REPL (Read-Eval-Print Loop ou Laço de Leitura-Avaliação-Impressão) permite interagir ao vivo com o código; ele lê a entrada, avalia, imprime o resultado e retorna ao início. Programadores usam isso para testar trechos de código, depurar e aprender novas linguagens em tempo real.

## Como fazer:
Go não inclui um REPL embutido, mas você pode usar ferramentas de terceiros. Uma ferramenta popular é o `gore`:

```go
// Instale o gore usando
$ go install github.com/motemen/gore/cmd/gore@latest

// Execute o gore
$ gore
gore version 0.5.0  :help para ajuda
gore> :import fmt
gore> fmt.Println("Hello, Go REPL!")
Hello, Go REPL!
nil
```

## Mergulho Profundo
Originalmente desenvolvidos para Lisp, REPLs são comuns em linguagens dinâmicas como Python ou Ruby. Go, sendo de tipagem estática, não inclui um de forma nativa. Alternativas ao `gore` incluem `go-pry` e `yaegi`. Essas ferramentas interpretam o código Go, permitindo explorar e validar ideias rapidamente sem compilar um aplicativo completo. Elas são especialmente úteis para iniciantes e em contextos educacionais onde o foco está na aprendizagem e experimentação.

## Veja Também
- `gore`: [https://github.com/motemen/gore](https://github.com/motemen/gore)
- `go-pry`: [https://github.com/d4l3k/go-pry](https://github.com/d4l3k/go-pry)
- `yaegi`: [https://github.com/traefik/yaegi](https://github.com/traefik/yaegi)
