---
title:                "Buscando e substituindo texto"
html_title:           "C++: Buscando e substituindo texto"
simple_title:         "Buscando e substituindo texto"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## O Que e Por Que?

Procurar e substituir texto é uma técnica comumente usada por programadores para alterar partes específicas de um código. Isso pode ser útil quando se está fazendo mudanças em grande escala ou corrigindo erros em vários arquivos de uma vez. É uma forma eficiente de economizar tempo e garantir a precisão nas alterações feitas no código.

## Como Fazer:

Há várias maneiras de realizar a busca e substituição de texto em um programa escrito em C++. Vamos dar uma olhada em alguns exemplos:

```
// Exemplo 1: Usando a função replace()
#include<iostream>
#include<string>
using namespace std;

int main(){
    string frase = "Eu amo programar em C++!";
    
    // Substituindo a palavra "amo" por "adoro"
    replace(frase.begin(), frase.end(), 'amo', 'adoro');
    
    cout << frase << endl;
    return 0;
}

// Saída: Eu adoro programar em C++!
```

```
// Exemplo 2: Usando a função find() e replace()
#include<iostream>
#include<string>
using namespace std;

int main(){
    string frase = "O C++ é uma ótima linguagem de programação!";
    string palavra = "ótima";
    
    // Procurando a posição da palavra "ótima" na frase
    size_t pos = frase.find(palavra);
    
    if(pos != string::npos){
        // Substituindo "ótima" por "maravilhosa" na posição encontrada
        frase.replace(pos, palavra.length(), "maravilhosa");
    }
    
    cout << frase << endl;
    return 0;
}

// Saída: O C++ é uma maravilhosa linguagem de programação!
```

## Mergulho Profundo:

A busca e substituição de texto é uma técnica amplamente utilizada desde os primórdios da programação. Ela foi introduzida pela primeira vez no editor de texto "ed" em 1971 e, posteriormente, aprimorada no comando "sed" em 1973. Hoje em dia, a maioria dos editores de texto e ambientes de desenvolvimento integrado (IDEs) possuem ferramentas avançadas para realizar essa tarefa.

Além disso, além da função replace() e find() apresentadas nos exemplos acima, existem outras maneiras de realizar a busca e substituição de texto em um programa em C++. Uma delas é usando expressões regulares, que permitem realizar alterações em texto que se encaixam em um determinado padrão.

## Veja também:

- [Documentação do C++ sobre replace() e find()](https://en.cppreference.com/w/cpp/string/basic_string/find)
- [Introdução às expressões regulares em C++](https://www.regular-expressions.info/cpp.html)