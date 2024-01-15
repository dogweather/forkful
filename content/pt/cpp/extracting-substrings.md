---
title:                "Extraindo Substrings"
html_title:           "C++: Extraindo Substrings"
simple_title:         "Extraindo Substrings"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que
Você já precisou extrair uma parte específica de uma string em seu código? Talvez você precise pegar apenas alguns caracteres de um endereço de e-mail ou de um número de telefone. É aí que entra a extração de substrings! Neste artigo, vamos aprender como fazer isso utilizando a linguagem de programação C++.

## Como Fazer
Para extrair substrings em C++, utilizamos a função "substr()", que faz parte da biblioteca padrão <string>. Veja o exemplo abaixo:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {

    // Definindo uma string
    string frase = "Este é um exemplo de frase.";

    // Extraindo uma substring da posição 5 até a posição 11
    string substring = frase.substr(5, 11);

    // Imprimindo a substring
    cout << substring << endl;

    return 0;
}
```

O resultado deste código será "é um exemplo", uma vez que o primeiro parâmetro da função substr() é a posição inicial da substring e o segundo é o tamanho que queremos que ela tenha. A partir disso, podemos trabalhar com a substring da mesma forma que trabalhamos com uma string normal.

## Deep Dive
Além de extrair substrings com posições e tamanhos específicos, também é possível utilizar a função substr() para extrair uma substring até o final da string original. Isso é feito deixando o segundo parâmetro em branco, como no exemplo a seguir:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {

    // Definindo uma string
    string frase = "Este é outro exemplo de frase.";

    // Extraindo uma substring da posição 17 até o final
    string substring = frase.substr(17);

    // Imprimindo a substring
    cout << substring << endl;

    return 0;
}
```

O resultado deste código será "exemplo de frase.", já que a substring começa na posição 17 e vai até o final da string original.

## Veja Também
- [Documentação oficial da função substr() em C++](https://www.cplusplus.com/reference/string/string/substr/)
- [Tutorial sobre strings em C++](https://www.geeksforgeeks.org/stdstring-class-in-c/)