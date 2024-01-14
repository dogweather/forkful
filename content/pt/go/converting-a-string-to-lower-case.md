---
title:                "Go: Convertendo uma string para caixa baixa"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Por que converter uma string para letras minúsculas em Go?

Ao trabalhar com strings em Go, pode ser necessário converter todas as letras para minúsculas em algum momento. Isso é especialmente importante quando se trata de entrada do usuário, pois é importante garantir que as comparações de strings sejam feitas corretamente, independentemente do caso das letras.

# Como fazer isso em Go?

Converter uma string para letras minúsculas em Go é bastante simples. Basta usar a função incorporada `strings.ToLower()` e passar a string desejada como argumento. Veja um exemplo abaixo:

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    original := "TEXTO DE EXEMPLO"
    lower := strings.ToLower(original)
    fmt.Println("String original:", original)
    fmt.Println("String convertida para minúsculas:", lower)
}
```

A saída deste exemplo seria a seguinte:

```
String original: TEXTO DE EXEMPLO
String convertida para minúsculas: texto de exemplo
```

# Mergulho profundo

A função `strings.ToLower()` em Go utiliza a tabela de conversão ASCII para converter as letras maiúsculas para minúsculas. Isso significa que apenas caracteres ASCII serão convertidos corretamente para minúsculas. Se a string contiver caracteres não ASCII, o resultado pode ser imprevisível. Portanto, é importante garantir que a string que você está convertendo contenha apenas caracteres ASCII para obter um resultado confiável.

# Veja também

- [Documentação oficial sobre strings em Go (em inglês)](https://golang.org/pkg/strings/)
- [Tutorial sobre strings em Go (em português)](https://www.calhau.me/strings-em-go/)
- [Exemplos de conversão de strings em Go (em inglês)](https://programming.guide/go/convert-string-to-lowercase-letters.html)