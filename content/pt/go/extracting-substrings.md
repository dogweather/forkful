---
title:                "Extraindo substrings"
html_title:           "Bash: Extraindo substrings"
simple_title:         "Extraindo substrings"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/extracting-substrings.md"
---

{{< edit_this_page >}}

---

## O quê e Por quê?

Extrair substrings é o processo de selecionar uma sequência específica de caracteres de uma string. Programadores fazem isso para manipular, analisar ou transformar dados de string.

## Como Fazer:

No Go, é bastante simples extrair substrings. Aqui está um exemplo:

```Go
package main

import "fmt"

func main() {
	str := "Olá, Mundo do Programa"
	subStr := str[7:12]
	fmt.Println(subStr)  // Imprime "Mundo"
}
```

Nesse exemplo, estamos criando uma string chamada "str" e, em seguida, usando a sintaxe `[x:y]` para pegar a substring que começa na 7ª posição e termina antes da 12ª posição (baseado em zero). O resultado é a palavra "Mundo". 

## Mergulho Profundo

Historicamente, a extração de substrings é uma tarefa comum em muitas linguagens de programação. É particularmente útil para processamento de texto, análise de logs, ou construção de respostas de API.

No Go, especificamente, a extração de substrings é feita em tempo constante devido à implementação de strings como um fatia de bytes. Isso significa que a operação é muito rápida, mas deve-se tomar cuidado para não extrair substrings que excedam os limites da string, pois isso resultará em um erro de tempo de execução.

Existem alternativas à extração de substrings incorporadas, se necessário, como o pacote `strings` que fornece funções como `strings.Split()` e `strings.TrimSpace()`. No entanto, a abordagem incorporada com o índice de fatia é geralmente o método mais direto e performático.

## Veja Também

Para saber mais sobre manipulação de strings em Go, confira:

- Documentação Oficial do Go: https://golang.org/pkg/strings/
- Um tutorial abrangente sobre Strings em Go: https://www.callicoder.com/golang-string/
- Um vídeo explicativo profundo sobre strings em Go: https://www.youtube.com/watch?v=7UQBMb8ZpuE

Lembre-se, a prática é fundamental para se tornar proficiente na manipulação de strings. Continue praticando!