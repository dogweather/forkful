---
title:                "Convertendo uma string para minúsculas"
html_title:           "C++: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que

Há diversas situações em que pode ser necessário converter uma string para letras minúsculas em um programa em C++. Por exemplo, em um programa que solicita a entrada de um usuário para criar uma senha, pode ser importante converter as letras para minúsculas para garantir que não haja erros de digitação na hora de verificar a senha.

## Como Fazer

Para converter uma string para minúsculas em C++, podemos utilizar a função `tolower()` da biblioteca `<cctype>`. Veja um exemplo abaixo:

```C++
#include <iostream>
#include <string>
#include <cctype>

using namespace std;

int main() {

    // String de exemplo
    string texto = "Esta é Uma String de Exemplo";
    
    // Loop para percorrer todos os caracteres da string
    for (int i = 0; i < texto.length(); i++) {
        // Converte o caracter atual para minúscula e o substitui na string original
        texto[i] = tolower(texto[i]);
    }
    
    // Imprime a string convertida
    cout << texto << endl;
    
    return 0;
}
```

O output deste programa seria:

```
esta é uma string de exemplo
```

## Deep Dive

A função `tolower()` é definida no cabeçalho `<cctype>` e sua implementação é feita utilizando a tabela ASCII. Ela basicamente verifica se o caracter passado como parâmetro é uma letra maiúscula e, se for, retorna o equivalente em letras minúsculas. Caso contrário, o próprio caracter é retornado. É importante ressaltar que essa função não faz verificação de acentos ou caracteres especiais, apenas converte letras de A a Z.

## Veja Também

- Documentação oficial da função `tolower()`: http://www.cplusplus.com/reference/cctype/tolower/
- Mais informações sobre a tabela ASCII: https://pt.wikipedia.org/wiki/ASCII