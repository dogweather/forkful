---
title:                "C++: Excluindo caracteres que correspondem a um padrão"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que

Há muitas razões pelas quais alguém pode querer deletar certos caracteres que se encaixam em um padrão em seu código C++. Pode ser para limpar a formatação, substituir pedaços de código antigo ou até mesmo para melhorar a eficiência e desempenho do programa. Em geral, a exclusão de caracteres é uma tarefa útil e necessária na programação.

## Como Fazer

Para deletar caracteres em C++, podemos usar a função `std::remove_if` da biblioteca `<algorithm>`. Esta função recebe dois parâmetros: o primeiro é o início do intervalo de caracteres que será verificado, e o segundo é o final desse intervalo. Além disso, é necessário um predicado que será usado para determinar quais caracteres serão excluídos. Na maioria dos casos, podemos usar o `isspace` ou `isdigit` da biblioteca `<cctype>` para excluir espaços em branco ou dígitos numéricos.

Aqui está um exemplo de código que exclui todos os espaços em branco de uma string:

```C++
#include <iostream>
#include <algorithm>
#include <cctype>

int main() {
    std::string str = "Hello World!";
    str.erase(std::remove_if(str.begin(), str.end(), isspace), str.end());
    std::cout << str << std::endl;
    return 0;
}
```

A saída será `HelloWorld!` sem os espaços em branco.

Outra maneira de deletar caracteres é usando um laço `for` para percorrer a string e excluir cada caractere que corresponder ao padrão.

## Aprofundando

A função `std::remove_if` não exclui realmente os caracteres, mas os move para o final da string. O que ela retorna é um iterador apontando para o início dos caracteres que foram excluídos. Para realmente excluir esses caracteres da string, podemos usar a função `erase` da classe `std::string`.

É importante ressaltar que, ao excluir caracteres de uma string, a posição de cada caractere após os excluídos será alterada. Por isso, devemos sempre usar a função `erase` após a função `std::remove_if` para garantir que os caracteres sejam excluídos da posição correta.

## Veja também

- Documentação oficial da função `std::remove_if` na C++ Reference: https://en.cppreference.com/w/cpp/algorithm/remove
- Exemplo de código em C++ para deletar caracteres: https://www.geeksforgeeks.org/clear-the-screen-using-c/
- Tutorial em vídeo sobre como deletar caracteres em C++: https://www.youtube.com/watch?v=Y9nGChsES68