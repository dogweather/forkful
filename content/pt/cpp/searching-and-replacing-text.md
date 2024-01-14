---
title:    "C++: Buscando e substituindo texto"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que

Se você já trabalhou com programação, provavelmente já se deparou com a necessidade de substituir um determinado trecho de texto por outro. Através da função "search and replace" é possível automatizar esse processo e poupar tempo e esforço na hora de editar seu código.

## Como fazer

Para realizar a substituição de texto em C++, é necessário utilizar a função "replace" da biblioteca <algorithm>. Essa função recebe três parâmetros, sendo o primeiro o início da string, o segundo o fim da string e o terceiro é o texto que será substituído.

```
#include <iostream>
#include <algorithm>

using namespace std;

int main(){
    
    // Definindo a string original
    string texto = "Hello, world!";
    
    // Realizando a substituição
    replace(texto.begin(), texto.end(), 'o', 'a');
    
    // Imprimindo o resultado
    cout << texto << endl;
    
    return 0;
}
```

**Saída:**

"Hella, warld!"

Esse é apenas um exemplo básico de como realizar a substituição de texto em C++. É importante ter em mente que a função "replace" substitui todas as ocorrências do texto na string, portanto, se necessário, deve-se utilizar a função "find" para procurar a posição exata do trecho que será substituído.

## Mergulho profundo

Além da função "replace", a biblioteca <algorithm> possui diversas outras funções que podem auxiliar na manipulação de strings, tais como "find", "find_if", "count" e "erase". É fundamental ter domínio dessas funções para realizar a substituição de texto de forma eficaz e sem erros.

É importante ressaltar que, apesar de ser uma funcionalidade simples, a substituição de texto é muito utilizada em programas mais complexos, como editores de texto e compiladores. Portanto, é crucial entender e dominar as técnicas de busca e substituição em C++.

## Veja também

- [Função "replace" na documentação da biblioteca <algorithm>](https://en.cppreference.com/w/cpp/algorithm/replace)
- [Tutorial sobre manipulação de strings em C++](https://www.geeksforgeeks.org/string-manipulation-in-c-2/)
- [Outras funções da biblioteca <algorithm>](https://en.cppreference.com/w/cpp/algorithm)