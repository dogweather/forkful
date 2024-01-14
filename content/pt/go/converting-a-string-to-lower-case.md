---
title:    "Go: Convertendo uma string para minúsculas"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por Que

Convertendo uma string para letras minúsculas no Go pode ser muito útil em diversas situações, desde validação de formulários até a manipulação de dados em um banco de dados. Saber como realizar essa conversão pode facilitar e acelerar o desenvolvimento de aplicações em Go.

## Como Fazer

Usando a função `ToLower()` da biblioteca `strings`, podemos facilmente converter uma string para letras minúsculas. Veja um exemplo abaixo:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	str := "Olá, Mundo!"
	fmt.Println(strings.ToLower(str))
}
```

O código acima irá imprimir `olá, mundo!`, com todas as letras em minúsculo. É importante lembrar que essa função não altera a string original, mas retorna uma cópia convertida.

Outra opção é utilizar o pacote `unicode` e sua função `ToLower()`, que lida com caracteres Unicode corretamente:

```Go
package main

import (
	"fmt"
	"unicode"
)

func main() {
	str := "Ça Va?"
	fmt.Println(unicode.ToLower(str))
}
```

Este código irá imprimir `ça va?` corretamente, com o caractere 'Ç' convertido para 'ç'. É importante notar que o uso da função `ToLower()` sem o pacote `unicode` pode não ser suficiente para converter caracteres de outros idiomas corretamente.

## Deep Dive

Ao converter uma string para letras minúsculas, é importante considerar a diferença entre minúsculas e maiúsculas pode variar de acordo com o idioma. Por exemplo, em alguns idiomas, como o turco, os caracteres com acentos podem ter versões maiúsculas e minúsculas diferentes.

O pacote `unicode` do Go lida com essa variação de caracteres ao fornecer funções específicas para converter letras para um caso determinado. Além disso, também é possível converter apenas a primeira letra de uma string para minúscula, usando a função `ToLowerSpecial()` do pacote `unicode`, que permite especificar o idioma desejado.

## Veja Também

- [Documentação oficial do pacote strings](https://golang.org/pkg/strings/)
- [Documentação oficial do pacote unicode](https://golang.org/pkg/unicode/)
- [Artigo sobre internacionalização de aplicações em Go](https://blog.golang.org/micro-i18n)