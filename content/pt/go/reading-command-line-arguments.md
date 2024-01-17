---
title:                "Lendo argumentos da linha de comando"
html_title:           "Go: Lendo argumentos da linha de comando"
simple_title:         "Lendo argumentos da linha de comando"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## O que & Porquê?

Ler argumentos de linha de comando é uma parte importante da programação Go. Ao ler argumentos de entrada fornecidos pelo usuário, os programadores podem personalizar e controlar o comportamento do programa de acordo com as necessidades do usuário.

## Como fazer:

Ler argumentos de linha de comando em Go é simples e direto. Tudo o que você precisa fazer é usar a função ```os.Args``` que retorna uma slice contendo os argumentos fornecidos pelo usuário.

```Go
func main() {
    arguments := os.Args
    fmt.Println(arguments)
}
```
Ao rodar o programa e fornecer argumentos de linha de comando, a saída será uma slice com os argumentos fornecidos.

```
$ go run main.go argumento1 argumento2 argumento3
["main" "argumento1" "argumento2" "argumento3"]
```

## Mergulho Profundo:

A leitura de argumentos de linha de comando é uma técnica comum em muitas linguagens de programação, incluindo Go. Esses argumentos podem ser usados ​​para definir opções e parâmetros para o programa. Alternativas para leitura de argumentos de linha de comando incluem a leitura de arquivos de configuração ou a utilização de variáveis de ambiente. A implementação em Go é feita pela biblioteca padrão e é uma das maneiras mais eficientes e fáceis de ler argumentos de linha de comando.

## Veja também:

- Documentação oficial do pacote `os`: https://golang.org/pkg/os/
- Tutorial de leitura de argumentos de linha de comando em Go: https://gobyexample.com/command-line-arguments