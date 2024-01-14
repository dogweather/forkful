---
title:                "Go: Lendo argumentos da linha de comando"
simple_title:         "Lendo argumentos da linha de comando"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que ler argumentos da linha de comando em Go?

Ler argumentos da linha de comando é essencial para criar programas flexíveis e interativos em Go. Ao entender como ler e usar esses argumentos, você pode tornar seus programas mais dinâmicos e customizáveis para diferentes casos de uso.

## Como fazer:

Para ler argumentos da linha de comando em Go, é preciso primeiro importar o pacote "os". Em seguida, usar a função "os.Args" que retorna uma slice contendo os argumentos passados na linha de comando. Por exemplo:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	args := os.Args
	fmt.Println(args)
}
```

Se rodarmos este programa na linha de comando com alguns argumentos, veremos a seguinte saída:

```
$ go run main.go arg1 arg2 arg3
[arg1 arg2 arg3]
```

Agora, podemos acessar cada argumento individualmente a partir da slice retornada. Por exemplo, se quisermos imprimir apenas o segundo argumento, podemos fazer:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	args := os.Args
	fmt.Println(args[1])
}
```

A saída será:

```
$ go run main.go arg1 arg2 arg3
arg2
```

## Aprofundando:

Além da função "os.Args", existem outras formas de ler argumentos da linha de comando em Go. Você pode usar a função "flag" do pacote "flag" para criar argumentos com opções e flags. Também é possível usar a estrutura "os.Args" para acessar argumentos específicos por meio de índices ou usar a função "os.Getopt" para acessar opções mais complexas.

Veja alguns links úteis para aprofundar seu conhecimento sobre leitura de argumentos da linha de comando em Go:

- [Documentação oficial do pacote "flag"](https://golang.org/pkg/flag/)
- [Tutorial do pacote "flag" em inglês](https://www.digitalocean.com/community/tutorials/how-to-use-the-flag-package-in-go-pt)
- [Documentação oficial do pacote "os"](https://golang.org/pkg/os/)
- [Tutorial do pacote "os" em português](https://golang.org/doc/articles/syscall_pt_br.html)

## Veja também:

- [Documentação do pacote "flag"](https://golang.org/pkg/flag/)
- [Documentação do pacote "os"](https://golang.org/pkg/os/)