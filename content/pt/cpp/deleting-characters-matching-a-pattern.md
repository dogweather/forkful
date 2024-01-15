---
title:                "Deletando caracteres que correspondem a um padrão"
html_title:           "C++: Deletando caracteres que correspondem a um padrão"
simple_title:         "Deletando caracteres que correspondem a um padrão"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que

Você pode querer deletar certos caracteres de uma string por uma variedade de razões, como limpar dados de entrada ou restringir o formato de uma senha. Saber como fazer isso em C++ pode ser útil em muitos projetos de programação.

## Como Fazer

Para deletar caracteres em C++, você pode usar a função erase() da biblioteca <string> junto com um laço for para percorrer a string e excluir os caracteres desejados. Veja um exemplo abaixo:

```C++
#include <iostream>
#include <string>
using namespace std;

int main() {
    string str = "Exemplo de string!";
    char pattern = 'e';

    for(int i = 0; i < str.length(); i++){
        if(str[i] == pattern){
            str.erase(i,1);
        }
    }
    
    cout << str; //saída: "xmplo d string!"
    
    return 0;
}
```

Para deletar mais de um caractere, basta adicionar condições ao laço for. Você também pode usar a função remove_if() da biblioteca <algorithm> para excluir caracteres que correspondem a um predicado.

## Deep Dive

Ao usar a função erase(), tenha cuidado com o índice dos caracteres. Lembrando que o primeiro caractere de uma string tem o índice 0, então você precisa garantir que os índices estejam ajustados ao atualizar a string. Além disso, você pode armazenar o resultado em uma nova string ou atualizar a string original.

## Veja Também

- [Função erase() na documentação do C++](https://www.cplusplus.com/reference/string/string/erase/)
- [Função remove_if() na documentação do C++](https://www.cplusplus.com/reference/algorithm/remove_if/)