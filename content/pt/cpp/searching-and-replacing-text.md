---
title:                "C++: Buscando e substituindo texto"
simple_title:         "Buscando e substituindo texto"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que fazer uma busca e substituição de texto?

Fazer uma busca e substituição de texto é uma habilidade importante para todo programador. Isso permite que você encontre e substitua facilmente trechos de código em seus projetos, economizando tempo e aumentando a eficiência.

## Como fazer uma busca e substituição de texto no C++?

A busca e substituição de texto no C++ é bastante simples e pode ser realizada usando a função `str.replace()` da biblioteca `string`. Veja abaixo um exemplo de como usar essa função:

```C++
#include <iostream>
#include <string>

int main() {
    std::string texto = "Este é um exemplo de texto para substituição.";
    std::cout << "Texto original: " << texto << std::endl;
    
    texto.replace(11, 7, "exercício"); // substitui "exemplo" por "exercício"
    std::cout << "Texto modificado: " << texto << std::endl;
    
    return 0;
}
```

Output:
```
Texto original: Este é um exemplo de texto para substituição.
Texto modificado: Este é um exercício de texto para substituição.
```

Nesse exemplo, usamos a função `replace()` para substituir a palavra "exemplo" por "exercício". A função recebe três argumentos: a posição inicial da string a ser substituída, o número de caracteres a serem substituídos e a nova string a ser inserida.

## Mergulhando mais fundo

Além da função `replace()`, a biblioteca `string` também possui outras funções úteis para busca e substituição de texto, como `find()` e `insert()`. Além disso, existem bibliotecas adicionais, como a `regex`, que fornecem recursos mais avançados para a manipulação de strings. 

É importante entender também que a busca e substituição de texto pode ser útil não apenas para modificar trechos de código, mas também para tarefas como formatação de texto ou tratamento de dados em arquivos.

## Veja também

- [Documentação do C++ sobre a biblioteca string](https://www.cplusplus.com/reference/string/)
- [Tutorial completo sobre busca e substituição de texto em C++](https://www.programiz.com/cpp-programming/string-replace)