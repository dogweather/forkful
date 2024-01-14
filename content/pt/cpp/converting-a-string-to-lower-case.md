---
title:                "C++: Convertendo uma string para minúsculas"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que converter uma string para minúsculo

Muitas vezes, ao trabalhar com strings em C++, precisamos garantir que elas estejam em um formato específico. Uma das tarefas comuns é converter uma string para minúsculo, o que pode ser útil ao realizar comparações ou manipulações de texto. Neste artigo, vamos discutir por que essa conversão é necessária e como podemos realizar isso em nossos programas.

## Como fazer a conversão

Existem várias maneiras de converter uma string para minúsculo em C++. Aqui estão dois exemplos de código que mostram duas abordagens diferentes:

```C++
// usando um laço for
#include <iostream>
#include <cctype> // biblioteca para funções de caso
using namespace std;

int main() {
    string minhaString = "EXEMPLO";
    // laço for para iterar sobre cada caractere da string
    for (int i = 0; i < minhaString.length(); i++) {
        // usando a função tolower para converter para minúsculo
        minhaString[i] = tolower(minhaString[i]);
    }
    // impressão do resultado
    cout << minhaString << endl; // vai imprimir "exemplo"
    
    return 0;
}
```

```C++
// usando a função transform
#include <iostream>
#include <algorithm> // biblioteca para a função transform
#include <cctype> // biblioteca para funções de caso
using namespace std;

int main() {
    string minhaString = "EXEMPLO";
    // usar a função transform para converter para minúsculo
    transform(minhaString.begin(), minhaString.end(), minhaString.begin(), ::tolower);
    // impressão do resultado
    cout << minhaString << endl; // vai imprimir "exemplo"
    
    return 0;
}
```

Podemos ver que, em ambos os exemplos, primeiro importamos a biblioteca <cctype>, que contém a função tolower para converter um caractere para minúsculo. Em seguida, usamos um laço for ou a função transform para iterar sobre cada caractere da string e realizar a conversão. Em ambos os casos, o resultado é armazenado de volta na mesma string.

É importante lembrar que, ao manipular strings, sempre devemos verificar se todos os caracteres necessários estão disponíveis na biblioteca que estamos usando.

## Uma análise mais profunda

A conversão de uma string para minúsculo pode parecer uma tarefa simples, mas existem alguns detalhes importantes que devemos levar em consideração.

Um dos pontos a serem observados é que a conversão depende do idioma. Por exemplo, em alguns idiomas, a letra "i" maiúscula pode ser convertida para "ı" minúscula, enquanto em outros pode permanecer como "i". Isso é importante a ser considerado ao realizar comparações entre duas strings em C++.

Outra coisa a ser observada é que, quando usamos a função transform, podemos escolher entre convertê-la para minúsculo ou para maiúsculo usando a função tolower ou toupper, respectivamente.

## Veja também

- [Guia de Idiomas - Strings](https://www.cplusplus.com/reference/string/) 
- [Documentação da biblioteca cctype](https://www.cplusplus.com/reference/cctype/)