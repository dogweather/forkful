---
title:                "Encontrando o comprimento de uma string"
html_title:           "C: Encontrando o comprimento de uma string"
simple_title:         "Encontrando o comprimento de uma string"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Encontrar o comprimento de uma string em C++ é determinar quantos caracteres há nela, incluindo espaços e caracteres especiais. Os programadores fazem isso para manipulação de texto, loops, memória alocada, dentre outros.

## Como Fazer:
Aqui está um exemplo de como você pode obter o comprimento de uma string em C++.

```C++
#include<iostream>
#include<string>
using namespace std;

int main(){
   string str;
   cout<<"Digite uma string: ";
   getline(cin, str);
   cout<<"O comprimento da string é: "<< str.length();
   return 0;
}
```
Se você inserir a string "Ola, Mundo!", o programa retornará "O comprimento da string é: 12".

## Mergulho Profundo

Historicamente, em C++ original (também conhecido como C++98), as strings eram manipuladas como arrays de caracteres e terminadas por um caractere nulo. Para obter o comprimento de uma string, era necessário iterar através do array até encontrar o caractere nulo.

Quanto a alternativas, em C++ você também pode usar a função `size()` em vez de `length()`. Ambas executam a mesma ação, pois são sinônimos no padrão C++ (ambas retornam o número de bytes, não o número de caracteres).

Sobre a implementação, `length()` e `size()` são implementadas como funções de membro da classe std::string. Eles retornam o número de bytes, não o número de caracteres, o que importa quando sua string contém caracteres multibyte, como em Unicode ou UTF-8.

## Veja Também

1. [Documentação Oficial do C++](http://www.cplusplus.com/)
3. [Tutorial de Strings em C++ no Programiz](https://www.programiz.com/cpp-programming/strings)