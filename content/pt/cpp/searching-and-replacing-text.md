---
title:                "C++: Buscando e substituindo texto"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que pesquisar e substituir texto em programas C++?

Há muitas razões pelas quais alguém pode precisar pesquisar e substituir texto em programas C++. Talvez você esteja fazendo uma grande refatoração de código e precise alterar nomes de variáveis ​​por toda a base de código, ou talvez esteja atualizando uma biblioteca e precise substituir um determinado comando por um novo. Seja qual for o caso, pesquisar e substituir texto é uma tarefa comum e importante na programação. Neste artigo, vamos explorar como realizar essa tarefa usando C++.

## Como fazer

A realização de pesquisa e substituição de texto em C++ é relativamente simples e pode ser feita usando o cabeçalho <string> e a função `std::string::replace()`. Veja um exemplo abaixo:

```C++
#include <iostream>
#include <string>

int main() {
    std::string texto = "Este é um exemplo de texto para ser substituído.";
    texto.replace(8, 7, "exemplo"); // Substitui "um" por "exemplo"
    
    std::cout << texto << std::endl;
    
    return 0;
}

```

Este código irá produzir a seguinte saída:

```
Este é um exemplo de texto para ser substituído.
```

Quando se trata de substituir palavras inteiras em um texto, é importante especificar a posição inicial e o comprimento da palavra a ser substituída. No exemplo acima, `texto.replace(8, 7, "exemplo")` substitui a palavra "um", começando na posição 8 e com um comprimento de 7 caracteres, pela palavra "exemplo". Portanto, é importante garantir que você esteja substituindo a palavra correta no local correto.

## Mergulho profundo

Embora a função `std::string::replace()` seja útil para substituir texto em uma string, ela não é muito eficiente quando se trata de substituir várias ocorrências da mesma palavra em uma string grande. Nesse caso, pode ser necessário usar funções como `std::string::find()` e `std::string::substr()` para encontrar e extrair as ocorrências individuais e, em seguida, concatenar as novas strings com as substituições feitas.

Além disso, vale ressaltar que pesquisar e substituir texto pode ser uma tarefa delicada e propensa a erros. É importante ter cuidado ao especificar as posições e os comprimentos corretos ao realizar substituições em strings, pois isso pode afetar o funcionamento do seu código. Certifique-se de testar seu código exaustivamente antes de fazer alterações em sua base de código.

## Veja também

- [Documentação da função std::string::replace() em C++](https://www.cplusplus.com/reference/string/string/replace/)
- [Exemplos e explicações sobre como pesquisar e substituir texto em C++](https://www.techiedelight.com/replace-all-occurrences-word-string-cpp/)
- [Vídeo tutorial sobre como pesquisar e substituir texto em C++](https://www.youtube.com/watch?v=FJIJuhHm0rM)