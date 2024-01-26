---
title:                "Convertendo uma string para minúsculas"
date:                  2024-01-20T17:38:28.317342-07:00
model:                 gpt-4-1106-preview
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?
Converter uma string para letras minúsculas significa transformar todos os caracteres alfabéticos da string em sua forma minúscula. Programadores fazem isso para padronizar dados, facilitar comparações e operações de busca, já que "A" é diferente de "a" na maioria dos sistemas de computação.

## How to:
Em Go, a conversão de strings para letras minúsculas é feita usando a função `ToLower` do pacote `strings`. Aqui está como isso funciona:

```go
package main

import (
	"fmt"
	"strings"
)

func main() {
	original := "Olá, Mundo!"
	lowercase := strings.ToLower(original)
	fmt.Println(lowercase) // saída: "olá, mundo!"
}
```

## Deep Dive
Converter strings para minúsculas é uma prática comum desde os primórdios da programação. Em Go, a função `ToLower` lida com a variação de caracteres Unicode corretamente, o que não é um dado certo em linguagens mais antigas ou em implementações caseiras. 

Alternativas ao uso do `ToLower` envolvem escrever sua própria função (não recomendado devido à complexidade do Unicode) ou usar expressões regulares para substituir caracteres (mais lento e complexo).

Internamente, `ToLower` usa a tabelas de mapeamento de caracteres para encontrar a versão minúscula de cada caractere. Isso é mais complicado para caracteres fora do ASCII, como os com acentos ou os que estão em outros alfabetos, mas para a maioria dos textos em português, é direto e eficiente.

## See Also
- Documentação da função `ToLower` do Go: https://pkg.go.dev/strings#ToLower
- Uma exploração da codificação Unicode em Go: https://blog.golang.org/strings
- Página sobre Unicode em geral: http://unicode.org
