---
title:                "Procurando e substituindo texto"
html_title:           "C++: Procurando e substituindo texto"
simple_title:         "Procurando e substituindo texto"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que

Se você é um programador, é provável que já tenha se deparado com a tarefa de procurar e substituir texto em seus códigos. Felizmente, a linguagem de programação C++ oferece uma maneira fácil e eficiente para realizar essa tarefa.

## Como fazer

Para procurar e substituir texto em C++, utilizamos a função `replace()` da biblioteca padrão `string`, que permite especificar um texto a ser procurado e um texto para substituir as ocorrências desse texto. Veja um exemplo:

```C++
#include <iostream>
#include <string>

int main() 
{
    std::string texto = "Olá mundo!";
    std::cout << "Texto original: " << texto << std::endl;
    
    // Substituindo "mundo" por "amigo"
    texto.replace(texto.find("mundo"), 5, "amigo");
    std::cout << "Texto modificado: " << texto << std::endl;
    
    return 0;
}

// Saída:
// Texto original: Olá mundo!
// Texto modificado: Olá amigo!
```

Nesse exemplo, utilizamos a função `find()` para encontrar a posição do texto "mundo" dentro da string `texto` e, em seguida, a função `replace()` para substituí-lo por "amigo". É importante notar que a função `find()` retorna a posição da primeira ocorrência do texto e que a função `replace()` modifica diretamente a string. Portanto, é necessário atualizar a posição do texto a ser substituído após cada alteração.

## Deep Dive

A função `replace()` também permite substituir apenas uma ocorrência específica do texto, fornecendo um terceiro parâmetro opcional com o número de ocorrências a serem substituídas. Além disso, é possível utilizar a função `getline()` para ler uma linha inteira do console, caso desejemos que o usuário forneça o texto a ser procurado e substituído. Veja um exemplo:

```C++
#include <iostream>
#include <string>

int main() 
{
    std::string texto = "Hoje é um belo dia para programar!";
    std::cout << "Texto original: " << texto << std::endl;
    
    // Lendo o texto a ser procurado e substituído
    std::string procurar;
    std::cout << "Digite o texto a ser procurado: ";
    getline(std::cin, procurar);
    
    // Substituindo apenas a primeira ocorrência do texto
    texto.replace(texto.find(procurar), procurar.length(), "programar em C++");
    std::cout << "Texto modificado: " << texto << std::endl;
    
    return 0;
}

// Saída:
// Texto original: Hoje é um belo dia para programar!
// Digite o texto a ser procurado: belo dia
// Texto modificado: Hoje é um programar em C++ para programar!
```

## Veja também

- [Documentação oficial do C++](https://cplusplus.com/)
- [Tutorial de C++ da programiz](https://www.programiz.com/cpp-programming)