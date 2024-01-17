---
title:                "Utilizando expressões regulares"
html_title:           "C++: Utilizando expressões regulares"
simple_title:         "Utilizando expressões regulares"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Regular Expressions (ou Expressões Regulares) são ferramentas poderosas que os programadores utilizam para encontrar padrões específicos em um texto. Elas são úteis para validar entradas de usuário, pesquisar e filtrar dados e até mesmo substituir informações em um documento. Além disso, eles economizam tempo e esforço, já que permitem que você processe grandes quantidades de dados de forma rápida e eficiente.

## How to:
Um exemplo simples de como usar expressões regulares em C ++ é através da biblioteca `std::regex`. Ela oferece vários métodos para trabalhar com strings e analisar padrões em um texto.
```
#include <iostream>
#include <regex>
using namespace std;

int main(){
  // Definindo o padrão de busca
  regex pattern ("[0-9]+"); 
  // String de entrada 
  string text = "Hoje é dia 15 de agosto, faltam 16 dias para o meu aniversário!";
  
  // Buscando padrão na string
  smatch results;
  regex_search(text, results, pattern);  

  // Imprimindo o primeiro resultado encontrado
  cout << results[0] << endl;

  // Substituindo o primeiro resultado por asteriscos
  string new_text = regex_replace(text, pattern, "***");
  
  // Imprimindo o novo texto
  cout << new_text << endl;

  return 0;
}
```
A saída desse código será:
```
15
Hoje é dia ** de agosto, faltam ** dias para o meu aniversário!
```

## Deep Dive:
As expressões regulares têm sido utilizadas desde a década de 1950 e foram popularizadas pelo linguista norte-americano Stephen Kleene. Há diversas variações e implementações de expressões regulares em diferentes linguagens de programação, mas o conceito básico permanece o mesmo.

Alternativas às expressões regulares incluem o uso de funções de string, como `find()` e `replace()`, ou o uso de algoritmos de busca tradicionais. Embora essas opções possam ser eficazes em alguns casos, as expressões regulares oferecem uma sintaxe mais poderosa e flexível para lidar com padrões em textos.

## See Also:
- [C++ Regular Expressions - cppreference.com](https://en.cppreference.com/w/cpp/regex)
- [The Theory of Regular Expression - regular-expressions.info](https://www.regular-expressions.info/)
- [Funções de string em C++ - cplusplus.com](http://www.cplusplus.com/reference/string/string/)