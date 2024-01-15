---
title:                "Comparando duas datas."
html_title:           "C++: Comparando duas datas."
simple_title:         "Comparando duas datas."
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que comparar duas datas?

Comparar datas é uma tarefa muito comum em programação e pode ser útil em diversas situações, como verificar a validade de um documento, calcular a diferença entre duas datas ou ordenar eventos cronologicamente. Aprender a comparar datas em C++ pode oferecer uma maior compreensão de como lidar com dados temporais em seus projetos.

## Como fazer a comparação de datas em C++?

Para comparar duas datas em C++, você precisa utilizar a biblioteca padrão <ctime> e seus métodos específicos para manipulação de tempo e datas. O exemplo abaixo mostra como comparar duas datas fornecidas pelo usuário e imprimir o resultado:

```C++
#include <iostream>
#include <ctime>

using namespace std;

int main() {
  // Exemplo de comparação de datas
  struct tm data1, data2;
  int diferenca;

  // Recebe os valores de dia, mês e ano para a primeira data
  cout << "Insira o dia da primeira data: ";
  cin >> data1.tm_mday;
  cout << "Insira o mês da primeira data: ";
  cin >> data1.tm_mon;
  cout << "Insira o ano da primeira data: ";
  cin >> data1.tm_year;

  // Recebe os valores de dia, mês e ano para a segunda data
  cout << "Insira o dia da segunda data: ";
  cin >> data2.tm_mday;
  cout << "Insira o mês da segunda data: ";
  cin >> data2.tm_mon;
  cout << "Insira o ano da segunda data: ";
  cin >> data2.tm_year;

  // Calcula a diferença entre as datas em dias
  diferenca = difftime(mktime(&data1), mktime(&data2)) / (60 * 60 * 24);

  // Imprime o resultado
  cout << "A diferença entre as datas é de " << diferenca << " dias." << endl;

  return 0;
}
```

Exemplo de entrada e saída:

```
Insira o dia da primeira data: 10
Insira o mês da primeira data: 8
Insira o ano da primeira data: 2021
Insira o dia da segunda data: 20
Insira o mês da segunda data: 8
Insira o ano da segunda data: 2021
A diferença entre as datas é de -10 dias.
```

O código acima utiliza a função `difftime()` para calcular a diferença de tempo em segundos entre as duas datas e, em seguida, divide esse valor pelo número de segundos em um dia para obter a diferença em dias.

## Aprofundando na comparação de datas em C++

Para a comparação de datas em C++, é importante entender como a estrutura `struct tm` funciona. Ela armazena valores referentes à data e hora em variáveis inteiras, como `tm_mday` para o dia do mês, `tm_mon` para o mês (0-11), `tm_year` para o ano (1900+), entre outros.

Além disso, é preciso ter cuidado na hora de comparar datas com anos bissextos, pois eles possuem 366 dias em vez de 365. Por isso, recomenda-se utilizar a função `mktime()` ao invés de `time()` para garantir que as datas sejam devidamente ajustadas.

## Veja também

- [Funções de tempo e data em C++ (em inglês)](https://www.cplusplus.com/reference/ctime/)
- [Tutorial sobre estruturas em C++ (em português)](https://www.cplusplus.com/doc/tutorial/structures/)