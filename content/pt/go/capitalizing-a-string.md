---
title:                "Capitalizando uma string"
html_title:           "Go: Capitalizando uma string"
simple_title:         "Capitalizando uma string"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Na programação, capitalizar uma string significa alterar todas as primeiras letras de cada palavra para maiúsculas. Programadores fazem isso principalmente para questões de apresentação e formatação de dados.

## Como Fazer:
Com Go, podemos capitalizar uma string usando a função `strings.Title()`. Aidna assim, essa função pode não funcionar corretamente para palavras em português com acentos. Vamos ver um exemplo:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	texto := "olá, mundo!"
	
	textoCapitalizado := strings.Title(texto)
	
	fmt.Println(textoCapitalizado)
}
```

Esse código retornará: "Olá, Mundo!".

## Aprofundamento
Na maioria dos idiomas, a capitalização de strings é uma questão trivial. Entretanto, em idiomas como o português que utilizam acentos e cedilhas, pode não ser tão simples. Por esse motivo, Go tem suporte incorporado para converter caracteres UTF-8, tornando-o adequado para lidar com idiomas complexos.

Se a função `strings.Title()` não for suficiente, você pode usar bibliotecas de terceiros, como a "go-humanize".

A implementação de capitalização de strings em Go é baseada na tabela de caracteres Unicode. Para cada ponto de código Unicode em uma string, a função verifica se o ponto de código é o início de uma nova palavra. Se for, a função converterá o ponto de código para maiúsculo.

## Veja Também
1. Documentação oficial Go para a biblioteca `strings`: https://golang.org/pkg/strings/
2. Tutorial Go: Strings, bytes, runes e characters em Go: https://blog.golang.org/strings
3. Biblioteca "go-humanize" no GitHub: https://github.com/dustin/go-humanize