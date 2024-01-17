---
title:                "Colocando as Letras em Maiúsculas: Uma Introdução à Programação de Computadores"
html_title:           "C++: Colocando as Letras em Maiúsculas: Uma Introdução à Programação de Computadores"
simple_title:         "Colocando as Letras em Maiúsculas: Uma Introdução à Programação de Computadores"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## O Que & Por Que?

Capitalizar uma string é transformá-la em uma representação na qual a primeira letra de cada palavra é maiúscula. Os programadores fazem isso para facilitar a leitura e melhorar a organização do código.

## Como:

```C++
#include <iostream>
#include <string>
#include <algorithm>

using namespace std;

int main(){
    string texto = "esse é um exemplo de string capitalizada";
    
    // usando a função transform da biblioteca algorithm
    transform(texto.begin(), texto.end(), texto.begin(), ::toupper);
    cout << texto << endl; // Saída: ESSE É UM EXEMPLO DE STRING CAPITALIZADA
    
    // também é possível usar um loop para percorrer a string 
    // e modificar as letras individualmente
    for (int i = 0; i < texto.length(); i++){
        if (texto[i] >= 'a' && texto[i] <= 'z') // verificando se é uma letra minúscula
            texto[i] = texto[i] - 32; // subtraindo 32 para obter a letra maiúscula correspondente
    }
    cout << texto << endl; // Saída: ESSE É UM EXEMPLO DE STRING CAPITALIZADA
    
    return 0;
}
```

## Mergulho Profundo:

Capitalizar strings tem sua origem no estilo de escrita CamelCase, popularizado pela linguagem de programação Java. Ele foi introduzido para facilitar a leitura e tornar o código mais legível, visto que muitas palavras compostas são utilizadas na programação. Além disso, existem outras formas de capitalização, como a capitalização apenas da primeira letra da primeira palavra, ou a capitalização de todas as letras. A escolha depende da preferência do programador e do estilo adotado pelo projeto.

## Veja Também:

- https://www.learncpp.com/cpp-tutorial/capitalization/
- https://pt.wikipedia.org/wiki/CamelCase
- https://docs.microsoft.com/pt-br/cpp/standard-library/toupper