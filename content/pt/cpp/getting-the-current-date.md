---
title:                "C++: Obtendo a data atual"
programming_language: "C++"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que obter a data atual em C++?

Existem vários motivos pelos quais você pode querer obter a data atual em um programa C++. Pode ser necessário registrar a data e hora de uma ação específica realizada pelo usuário, ou pode ser útil para fins de organização e gerenciamento de arquivos. Independentemente do motivo, a obtenção da data atual pode ser uma tarefa complicada para alguns programadores iniciantes. Neste artigo, vamos explorar como fazer isso de forma eficiente e sem complicações. 

## Como fazer isso?

A linguagem C++ possui uma biblioteca chamada "chrono" que possui funções específicas para lidar com datas e tempos. Para obter a data atual em C++, precisamos usar a função `now()` dentro do namespace `chrono`. Veja abaixo um exemplo de código que mostra como implementar isso em seu programa:

```C++
#include <iostream>
#include <chrono>

using namespace std;
using namespace chrono;

int main() {

  // Obtém a data e hora atual
  auto timeNow = system_clock::now();

  // Converte a data e hora atual para o formato "time_t" do C
  time_t currentTime = system_clock::to_time_t(timeNow);

  // Imprime a data atual
  cout << "A data atual é: " << ctime(&currentTime) << endl;

  return 0;
}
```

A saída deste código será algo como:

```
A data atual é: Thu Jan 16 14:37:10 2020
```

Você também pode customizar o formato da data de acordo com suas preferências, utilizando as funções disponíveis na biblioteca `chrono`. Leia a documentação para aprender mais sobre as opções disponíveis.

## Mergulho profundo

Na seção anterior, vimos como obter a data atual em C++ utilizando a biblioteca `chrono`. No entanto, é importante entender como essa função funciona por trás dos bastidores. 

A função `now()` retorna um objeto do tipo `std::chrono::time_point`, que representa um ponto específico no tempo. Esse objeto é criado usando o relógio do sistema (por isso usamos `system_clock` na função `now()`). Este relógio é sincronizado com o relógio do sistema operacional e fornece um tempo de alta resolução.

Em seguida, usamos a função `to_time_t()` para converter este objeto de `time_point` para o formato `time_t` do C, que é mais fácil de manipular e imprimir. A partir daí, podemos usar a função `ctime()` para formatar a data e hora no formato desejado.

## Veja também

- [Documentação da biblioteca chrono](https://en.cppreference.com/w/cpp/chrono)
- [Tutorial de C++ da SoloLearn](https://www.sololearn.com/Course/C++/)