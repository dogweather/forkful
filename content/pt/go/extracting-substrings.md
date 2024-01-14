---
title:                "Go: Extraindo subcadeias"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/extracting-substrings.md"
---

{{< edit_this_page >}}

# Por que extrair substrings em Go é útil?

Extrair substrings de uma string maior pode ser muito útil em várias situações de programação. Isso permite que você trabalhe com parte específicas de uma string, ao invés de ter que lidar com toda a string de uma vez.

## Como fazer em Go?

Felizmente, extrair substrings em Go é muito simples. Basta utilizar o método `Substr` da biblioteca `strings` e passar o índice inicial e final do trecho que você deseja extrair. Veja um exemplo:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	s := "Olá, mundo!"
	sub := strings.Substr(s, 5, 9)
	fmt.Println(sub)
}

// Output: mundo
```

Neste exemplo, utilizamos o método `Substr` para extrair a substring "mundo" a partir do índice 5 até o índice 9 da string "Olá, mundo!". Ou seja, apenas as letras "mundo" serão impressas na tela.

Você também pode utilizar os índices negativos para extrair substrings a partir do final da string. Por exemplo, se utilizarmos os índices `-5` e `-1`, o resultado será "mundo!" (excluindo o ponto de exclamação).

## Uma olhada mais profunda

Para aqueles que desejam aprender mais sobre a extração de substrings em Go, aqui estão algumas informações adicionais. O método `Substr` retorna uma nova string com o trecho especificado, sem modificar a string original. Além disso, o índice final é opcional, então se você não especificar um, a substring será extraída até o final da string.

Você também pode utilizar o método `Index` da biblioteca `strings` para obter os índices de início e fim de uma substring em uma string maior. Isso pode ser útil para casos em que você precisa encontrar uma substring em uma string grande e extrair apenas o trecho desejado.

# Veja também

Aqui estão alguns links úteis que podem ajudá-lo a aprender mais sobre como extrair substrings em Go:

- [Documentação oficial do método `Substr`](https://golang.org/pkg/strings/#Substr)
- [Exemplo de código do método `Substr`](https://play.golang.org/p/MN-WEpS4SUO)
- [Tutorial sobre como extrair substrings em Go](https://gobyexample.com/slicing)