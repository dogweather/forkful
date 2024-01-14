---
title:    "C++: Comparando duas datas"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Por que
Comparar duas datas é um processo comum em linguagens de programação, especialmente no desenvolvimento de aplicativos de gerenciamento de tarefas e agendas. Além disso, é útil ter essa habilidade para validar a entrada de datas em um programa.

## Como Fazer
Para comparar duas datas em C++, primeiro precisamos criar dois objetos de data e, em seguida, usar o operador de comparação (==, <, >) para verificar qual data é maior ou se são iguais. Por exemplo:

```C++
#include<iostream>
#include<ctime>
using namespace std;

int main() {
  time_t data1, data2;
  data1 = time(0);   // atribui a data atual ao primeiro objeto
  data2 = time(0) + 86400; //atribui a data de amanhã ao segundo objeto

  if (data1 > data2) { //verifica se a primeira data é maior que a segunda
    cout << "A primeira data é maior do que a segunda";
  }
  else if (data2 > data1) { //verifica se a segunda data é maior que a primeira
    cout << "A segunda data é maior do que a primeira";
  }
  else { //caso as datas sejam iguais
    cout << "As datas são iguais";
  }
  return 0;
}
```

Exemplo de saída: "A segunda data é maior que a primeira"

## Aprofundando
Para comparar duas datas com mais precisão, podemos usar a biblioteca `ctime` do C++. Ela fornece funções como `localtime` e `mktime` que nos permitem converter as datas em um formato mais legível e manipulável. Além disso, podemos usar a função `difftime` para obter a diferença em segundos entre duas datas.

Um ponto importante a ser considerado ao comparar datas é o sistema de data usado pelo computador, que pode variar entre diferentes regiões ou sistemas operacionais. Portanto, é importante ter certeza de que o sistema de data está corretamente configurado antes de realizar as comparações.

## Veja também
- [Documentação da biblioteca ctime](https://en.cppreference.com/w/cpp/header/ctime)
- [Como formatar datas em C++](https://www.cplusplus.com/reference/ctime/strftime/)
- [Validação de entrada de dados em C++](https://www.geeksforgeeks.org/validating-input-in-c-cabinets/)