---
title:                "Concatenando strings"
html_title:           "Elixir: Concatenando strings"
simple_title:         "Concatenando strings"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

# Concatenando Strings em C++

## O Que e Por Quê?

Concatenar strings é a prática de juntar duas ou mais strings em uma só. Os programadores fazem isso para manipular e combinar texto de forma eficaz no código.

## Como Fazer:

Em C++, nós podemos concatenar strings usando o operador `+` ou a função `append()`. Veja os exemplos abaixo:

```C++
// Usando o operador +
#include <iostream>
#include <string>

int main() {
   std::string s1 = "Olá, ";
   std::string s2 = "mundo!";
   std::string s3 = s1 + s2;

   std::cout << s3 << std::endl;  // Saída: Olá, mundo!
}
```

```C++
// Usando a função append()
#include <iostream>
#include <string>

int main() {
   std::string s1 = "Olá, ";
   std::string s2 = "mundo!";
   s1.append(s2);

   std::cout << s1 << std::endl;  // Saída: Olá, mundo!
}
```

## Deep Dive

Historicamente, em C, strings eram arrays de chars, e concatená-las exigia a função `strcat()`. Com a introdução do C++, a classe `string` foi criada e vieram com ela métodos mais eficientes e intuitivos para concatenar strings.

Existem outras maneiras de concatenar strings. Nós focamos no operador `+` e na função `append()` por serem mais usados. Outras funções, como `sprintf()`, também realizam a concatenação, mas de maneira menos intuitiva e com potencial de segurança reduzido.

Concatenar strings em C++ é na verdade um processo mais complexo do que parece. Quando você usa o operador `+` ou a função `append()`, o C++ cria uma nova string para manter o resultado. Isso implica em cópias de strings e movimentos de memória. Em grandes volumes, isso pode ser uma preocupação para a performance.

## Veja Também

"C++ Strings" no cppreference: [link](https://en.cppreference.com/w/cpp/string/basic_string)
"Concatenação de Strings em C++" no StackOverflow: [link](https://stackoverflow.com/questions/18892281/most-idiomatic-way-to-concatenate-strings-in-c)
"Classe string" na documentação oficial do C++: [link](http://www.cplusplus.com/reference/string/string/)