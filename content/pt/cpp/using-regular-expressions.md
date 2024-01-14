---
title:    "C++: Usando expressões regulares"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por que usar Expressões Regulares em C++?

Se você é um programador C++ em busca de uma maneira mais eficiente de lidar com a manipulação de strings, as expressões regulares podem ser a resposta para os seus problemas. Com seu poderoso conjunto de ferramentas, elas podem facilitar muito o seu trabalho, poupando tempo e tornando o código mais legível.

## Como Utilizar Expressões Regulares em C++

Antes de começar, é importante ter em mente que as expressões regulares são uma linguagem própria, com sua própria sintaxe e conjunto de regras. Por isso, pode ser um pouco intimidante para iniciantes. No entanto, com um pouco de prática e atenção a detalhes, é possível dominar essa técnica.

Para utilizar as expressões regulares em C++, é necessário incluir a biblioteca `<regex>` no início do código. Em seguida, é preciso criar um objeto `regex` com a expressão que se deseja buscar. Veja um exemplo abaixo:

```
#include <iostream>
#include <regex>

int main()
{
    // Expressão regular que busca por números inteiros
    std::regex regex("\\d+");

    // Texto a ser analisado
    std::string texto = "A linguagem C++ é formada por 0 e 1";

    // Objeto que armazenará o resultado da busca
    std::smatch resultado;

    // Utilizando a função `regex_search` para buscar pela expressão no texto
    if(std::regex_search(texto, resultado, regex))
    {
        // Imprimindo o resultado da busca
        std::cout << "Encontrado: " << resultado[0] << std::endl;
    }

    return 0;
}
```

O resultado desse código seria a impressão de "Encontrado: 0", mostrando que a expressão regular encontrou o número inteiro no texto.

É possível também utilizar as expressões regulares para substituir parte de um texto, utilizando a função `regex_replace`. Veja o exemplo abaixo:

```
#include <iostream>
#include <regex>

int main()
{
    // Expressão regular que substitui o primeiro número por um asterisco
    std::regex regex("\\d");

    // Texto a ser analisado
    std::string texto = "A linguagem C++ é formada por 0 e 1";

    // Utilizando a função `regex_replace` para substituir o primeiro número encontrado por um asterisco
    std::string resultado = std::regex_replace(texto, regex, "*");

    // Imprimindo o resultado da substituição
    std::cout << resultado << std::endl;

    return 0;
}
```

O resultado desse código seria a impressão de "A linguagem C++ é formada por * e 1".

## Aprofundando nas Expressões Regulares

As expressões regulares podem ser utilizadas para buscar ou substituir padrões específicos em textos, oferecendo uma maneira poderosa e flexível de lidar com strings em C++. Além das funções `regex_search` e `regex_replace`, existem outras ferramentas disponíveis na biblioteca `<regex>` que permitem a utilização de capturas (grupos de caracteres específicos) e a realização de buscas em múltiplas linhas de texto.

É importante notar que a sintaxe das expressões regulares pode variar ligeiramente entre as diferentes plataformas e compiladores de C++. Portanto, é sempre bom verificar a documentação da sua biblioteca de expressões regulares para garantir a compatibilidade do seu código.

## Veja também

- [Documentação da biblioteca `<regex>` em C++](https://en.cppreference.com/w/cpp/regex)
- [Tutorial de expressões regulares em C++](https://www.learncpp.com/cpp-tutorial/12-4-stdregex-how-to-use-regular-expressions-to-parse-text/)