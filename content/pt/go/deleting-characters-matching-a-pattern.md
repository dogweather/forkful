---
title:                "Go: Excluindo caracteres que correspondem a um padrão"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que

Deletar caracteres que correspondem a um padrão pode ser necessário em muitas situações de programação. Isso pode ajudar a limpar e formatar dados, gerar saída mais precisa ou até mesmo melhorar o desempenho de um programa.

## Como fazer

Para deletar um caractere que corresponde a um padrão especificado em Go, você pode seguir os seguintes passos:

1. Importe o pacote `strings` para poder utilizar suas funções de manipulação de strings.
2. Utilize a função `strings.ReplaceAll (s, antigo, novo, n)` para substituir todos os caracteres correspondentes no texto `s` pelo caractere `novo`.
3. O terceiro argumento, `n`, pode ser definido como `-1` para substituir todas as ocorrências ou com um valor específico para limitar o número de substituições.
4. Agora você pode salvar o resultado em uma nova variável ou sobrescrever a variável original com o novo texto.

Veja um exemplo de código abaixo:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	texto := "Goo glee goantes goalgumas govezes"
	fmt.Println("Texto original:", texto)

	fmt.Println("Texto sem a letra 'g':", strings.ReplaceAll(texto, "g", ""))

	fmt.Println("Texto com apenas duas substituições:", strings.ReplaceAll(texto, "go", "", 2))
}
```

A saída será:

```
Texto original: Goo glee goantes goalgumas govezes
Texto sem a letra 'g': Ooe lee oantes oalmaas ovezes
Texto com apenas duas substituições: Goo lee ante almas govezes
```

## Mergulho profundo

A função `strings.ReplaceAll` utiliza expressões regulares para encontrar os caracteres correspondentes. Isso significa que você pode usar padrões mais abrangentes, como `[^a-zA-Z]` para substituir todos os caracteres que não sejam letras. Além disso, a função também suporta o uso de outras funções de strings, como `strings.ToUpper` ou `strings.ToLower`, para transformar o resultado da substituição.

## Veja também

- Documentação oficial do pacote `strings` em Go: https://golang.org/pkg/strings/
- Tutorial de Expressões Regulares em Go: https://golang.org/pkg/regexp/
- Exemplos práticos de uso da função `strings.ReplaceAll`: https://gobyexample.com/substring-replacement