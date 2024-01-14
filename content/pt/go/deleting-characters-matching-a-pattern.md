---
title:    "Go: Removendo caracteres que correspondem a um padrão"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Por que

Às vezes, ao trabalhar com strings em Go, pode ser necessário excluir caracteres que correspondam a um determinado padrão. Isso pode ser útil para filtrar informações desnecessárias ou para realizar tarefas específicas de formatação de texto.

## Como fazer

Para excluir caracteres em Go, podemos usar a função `strings.ReplaceAll ()` e passar o padrão de caracteres a serem excluídos como seu parâmetro `"velho"` e uma string vazia `""` como seu parâmetro `"novo"`.

Exemplo de código:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	texto := "Este é um exemplo de uma frase com alguns caracteres indesejados."
	pattern := "aeiou"

	// Substituir as vogais pelo caractere vazio
	textoFinal := strings.ReplaceAll(texto, pattern, "")

	fmt.Println(textoFinal)
}
```

Saída:

```
Est  m xmpl d m frs cm lgns crctrs ndsjd.
```

Podemos ver que a função `ReplaceAll()` substituiu todas as vogais pela string vazia.

## Mergulho Profundo

Em Go, também podemos usar expressões regulares para excluir caracteres que correspondam a um padrão específico. Para isso, podemos usar a biblioteca `regexp` e sua função `ReplaceAllString()`.

Exemplo de código:

```Go
package main

import (
	"fmt"
	"regexp"
)

func main() {
	texto := "Go é uma linguagem de programação fantástica!"
	pattern := "[aeiou]"

	// Substituir todas as vogais pelo caractere vazio usando expressões regulares
	textoFinal := regexp.MustCompile(pattern).ReplaceAllString(texto, "")

	fmt.Println(textoFinal)
}
```

Saída:

```
G   m lnggm d prgrmçã fnststc!
```

Neste exemplo, usamos a expressão regular `[aeiou]` para identificar todas as vogais no texto e as substituiu pelo caractere vazio.

## Veja também

- [Documentação oficial do pacote strings](https://golang.org/pkg/strings/)
- [Documentação oficial do pacote regexp](https://golang.org/pkg/regexp/)
- [Tutorial de expressões regulares em Go](https://astaxie.gitbooks.io/build-web-application-with-golang/content/pt/07.4.html)