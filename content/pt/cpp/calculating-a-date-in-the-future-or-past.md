---
title:    "C++: Calculando uma data no futuro ou passado"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que?

Há muitas situações em que precisamos calcular datas no futuro ou no passado em nosso código. Isso pode ser útil em tarefas como reserva de hotéis, agendamento de compromissos ou até mesmo em jogos. Entender como realizar esses cálculos pode ajudá-lo a escrever um código mais eficiente e preciso.

## Como fazer?

Para calcular uma data no futuro, podemos usar a função `std::tm` da biblioteca `ctime`. Primeiro, precisamos definir uma variável do tipo `std::tm` para representar a data atual. Em seguida, usamos a função `std::mktime` para converter essa data para um formato de tempo que possamos manipular.

```
#include <iostream>
#include <ctime>

int main() {
  std::tm data_atual = {0};
  
  // definindo a data atual
  data_atual.tm_year = 121; // ano atual-1900
  data_atual.tm_mon = 08; // mês atual (começando em 0)
  data_atual.tm_mday = 24; // dia atual
  
  // convertendo para um formato manipulável
  std::time_t temp = std::mktime(&data_atual);
  ```
  
Agora, para calcular uma data no futuro, basta adicionar o número de dias desejado a essa variável `temp` e usar a função `std::localtime` para convertê-la de volta para o formato `std::tm`.

```
// adicionando 5 dias para a data atual
temp += 5 * 24 * 60 * 60;

// convertendo de volta para std::tm
std::tm data_futura = *std::localtime(&temp);

// imprimindo a nova data no formato DD/MM/AAAA
std::cout << "Data: " << data_futura.tm_mday << '/' << data_futura.tm_mon + 1 << '/' << data_futura.tm_year + 1900 << '\n';

return 0;
```

O código acima irá produzir o seguinte resultado:

```
Data: 29/9/2021
```

Para calcular uma data no passado, basta subtrair o número de dias desejado da variável `temp` e seguir os mesmos passos.

## Mergulho Profundo

Embora o método acima seja simples e direto, ele tem uma limitação: não considera a mudança de fuso horário ou horário de verão. Para contornar esse problema, podemos usar a função `std::mktime` com a ajuda da estrutura `std::timeinfo` para obter a data correta, levando em conta o fuso horário e horário de verão.

Outro ponto importante a ser considerado é que as datas passadas pelo usuário podem vir em formatos diferentes. Nesse caso, podemos usar a função `std::get_time` da biblioteca `iomanip` para obter uma data no formato `std::tm` a partir de uma string.

## Veja também

- [Documentação da função std::mktime](https://en.cppreference.com/w/cpp/chrono/c/mktime)
- [Documentação da função std::get_time](https://en.cppreference.com/w/cpp/io/manip/get_time)