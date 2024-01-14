---
title:    "Go: Capitalizando uma string"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Por que Capitalizar uma String em Go?

Capitalizar uma string é importante para garantir uma consistência visual e uma melhor leitura do texto no código. Além disso, strings capitalizadas são frequentemente usadas em nomes de variáveis, constantes e funções em Go, seguindo as práticas de estilo de codificação da linguagem.

## Como Capitalizar uma String em Go

Para capitalizar uma string em Go, podemos usar a função `strings.Title` do pacote "strings". Esta função recebe uma string como argumento e retorna a mesma string com a primeira letra de cada palavra em maiúscula. Por exemplo:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	str := "capitalizar esta string"
	capitalized := strings.Title(str)
	fmt.Println(capitalized)
}

// Saída: Capitalizar Esta String
```

Podemos usar esta função para capitalizar nomes de variáveis ou constantes, como no exemplo abaixo:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	const nomeCompleto = "joão silva"
	nome := strings.Title(nomeCompleto)
	fmt.Println(nome)
}

// Saída: João Silva
```

## Mergulho Profundo na Capitalização de Strings

Embora `strings.Title` seja a maneira padrão de capitalizar uma string em Go, existem outras formas de realizar essa tarefa. Uma alternativa é usar a função `strings.ToUpper` em combinação com `strings.ToLower` para converter a primeira letra em maiúscula e as demais em minúscula. Em casos mais específicos, podemos usar expressões regulares ou bibliotecas de terceiros para lidar com situações mais complexas.

## Veja Também

- [Documentação oficial do pacote "strings" em Go](https://golang.org/pkg/strings/)
- [Tutorial de expressões regulares em Go](https://blog.golang.org/regular-expressions)

Se você gostou deste artigo, confira também nosso post sobre [como formatar strings em Go](https://www.blogdoprogramador.com.br/formatando-strings-em-go). Happy coding!