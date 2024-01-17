---
title:                "Unindo strings"
html_title:           "C++: Unindo strings"
simple_title:         "Unindo strings"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## O que & Por quê?

Concatenação de strings é quando unimos duas ou mais strings para formar uma única string. Isso é útil para criar mensagens ou expressões mais longas. Programadores fazem isso para facilitar a manipulação de dados e a criação de mensagens mais complexas.

## Como fazer:

```C++
// Exemplo 1: Usando o operador de concatenação "+"
#include <iostream> 
using namespace std;

int main() {
    string saudacao = "Olá";
    string nome = "Maria";
    string mensagem = saudacao + " " + nome + "!"; // Usando o operador "+"
    cout << mensagem << endl; // Output: Olá Maria!

    // Exemplo 2: Usando a função concat()
    string texto1 = "Concatenando";
    string texto2 = " strings";
    string texto3 = texto1.concat(texto2); // Usando a função concat()
    cout << texto3 << endl; // Output: Concatenando strings

    return 0;
}
```

## Mergulho Profundo:

A concatenação de strings é uma técnica muito antiga na programação, remontando à época dos primeiros compiladores. Além do operador "+" e da função concat(), existem outras formas de concatenar strings, como o operador de atribuição "+=", que pode ser útil para atualizar valores de strings já existentes. No entanto, é importante lembrar que, ao contrário de outras linguagens de programação, C++ não permite a concatenação de strings com outros tipos de dados sem conversão explícita.

## Veja também:

- [Documentação C++ - Concatenação de Strings](https://en.cppreference.com/w/cpp/string/basic_string/concat)
- [Vídeo - Concatenação de Strings em C++](https://www.youtube.com/watch?v=jduMM8Nc6qA)
- [Tutorial - Aprendendo Strings em C++](https://www.cplusplus.com/articles/4z18T05o/)