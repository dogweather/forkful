---
title:    "Go: Encontrando o tamanho de uma string."
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Por que encontrar o comprimento de uma string em Go?

Às vezes, quando estamos escrevendo um programa em Go, pode ser necessário encontrar o comprimento de uma string. Isso pode ser útil em diversas situações, como realizar verificações de entrada ou manipular dados.

# Como fazer isso em Go?

Em Go, podemos encontrar o comprimento de uma string usando a função `len()`. Essa função é nativa da linguagem e pode ser aplicada diretamente em uma string, sem a necessidade de importar pacotes adicionais.

Por exemplo, se tivermos a string "Olá, mundo!", podemos encontrar seu comprimento da seguinte forma:

```Go
s := "Olá, mundo!"
fmt.Println(len(s))
```

A saída desse código será `12`, indicando que a string possui 12 caracteres.

# Passo a passo mais detalhado

Agora que sabemos como encontrar o comprimento de uma string em Go, vamos nos aprofundar um pouco mais no processo. Quando aplicamos a função `len()` em uma string, o que realmente acontece é que ela contabiliza o número de bytes que a compõem.

Isso significa que para uma string como "Olá", que possui quatro caracteres, o comprimento será de 7 bytes, já que cada caractere é representado por um byte e há ainda um byte extra para o caractere de acentuação "á".

É importante lembrar que em Go, uma string é uma sequência de bytes e, por isso, o número de bytes será igual ao comprimento da string.

# Veja também

- [Documentação oficial sobre a função len() em Go](https://golang.org/pkg/builtin/#len)
- [Artigo sobre manipulação de strings em Go](https://medium.com/rungo/string-data-type-in-go-8f25aefed9b6)
- [Stack Overflow thread sobre a diferença entre tamanho e comprimento de uma string em Go](https://stackoverflow.com/questions/16660197/golang-strings-why-is-the-byte-length-of-string-1)

See Also:

https://golang.org/pkg/builtin/#len
https://medium.com/rungo/string-data-type-in-go-8f25aefed9b6
https://stackoverflow.com/questions/16660197/golang-strings-why-is-the-byte-length-of-string-1