---
title:                "Interpolando uma string."
html_title:           "C++: Interpolando uma string."
simple_title:         "Interpolando uma string."
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/interpolating-a-string.md"
---

{{< edit_this_page >}}

## O que & Por quê?

Interpolar uma string em programação significa inserir valores de variáveis ou expressões em uma string. Programadores fazem isso para facilitar a criação e manipulação de strings dinâmicas, alterando as informações inseridas de acordo com as variáveis ou expressões utilizadas.

## Como fazer:

```C++
#include <iostream>
#include <string>

using namespace std;

int main(){
  string nome = "João";
  int idade = 25;
  
  //Interpolação simples
  cout << "Olá, meu nome é " << nome << " e tenho " << idade << " anos." << endl;
  
  //Interpolação com expressões
  int ano_atual = 2021;
  int ano_nascimento = ano_atual - idade;
  cout << "Eu nasci em " << ano_nascimento << ", um ano antes do ano atual." << endl;
  
  //Interpolação com formatação
  double salario = 2500.50;
  cout << "Meu salário atual é de R$" << fixed << setprecision(2) << salario << "." << endl;
  
  return 0;
}
```

Exemplo de saída:
```
Olá, meu nome é João e tenho 25 anos.
Eu nasci em 1996, um ano antes do ano atual.
Meu salário atual é de R$2500.50.
```

## Aprofundando:

- Contexto histórico: A interpolação de strings é uma técnica comum em linguagens de programação modernas, mas foi popularizada principalmente pela linguagem de programação Ruby.
- Alternativas: Em algumas linguagens, como Python e Java, a interpolação de strings pode ser feita utilizando o operador "+" entre strings e variáveis.
- Detalhes de implementação: Em C++, a interpolação de strings é feita utilizando o operador "<<" para inserir valores em uma string. É possível utilizar diferentes formatações para os valores inseridos, como no exemplo anterior com "fixed" e "setprecision".

## Veja também:

- Documentação oficial do C++: https://cplusplus.com/
- Artigo sobre interpolação de strings em Ruby: https://www.infoworld.com/article/3482009/rubys-famous-puts-youre-boring-too-here-s-your-girlfriend.html
- Comparação de diferentes maneiras de interpolar strings em diferentes linguagens: https://konstantinpavlov.net/blog/2018/04/27/string-interpolation-in-javascript-vs-c-plus-plus-vs-ruby-vs-python-vs-php/