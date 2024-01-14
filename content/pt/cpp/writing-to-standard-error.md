---
title:                "C++: Escrevendo para o erro padrão"
simple_title:         "Escrevendo para o erro padrão"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que escrever para o padrão de erro em C++?

Escrever para o padrão de erro em um programa C++ é uma ótima maneira de depurar e lidar com erros em tempo de execução. Ao enviar mensagens de erro para o padrão de erro, você pode identificar os problemas no seu código e tomar medidas para corrigi-los.

## Como escrever para o padrão de erro em C++

Escrever para o padrão de erro em C++ é simples e pode ser feito usando a função padrão `cerr`. Aqui está um exemplo de código e sua respectiva saída:

```C++
#include <iostream>
using namespace std;

int main() {
    cerr << "Este é um erro!" << endl;
    return 0;
}
```

Saída:

```
Este é um erro!
```

## Aprofundando no assunto

A função `cerr` é parte da biblioteca padrão `<iostream>` em C++ e é usada para enviar dados para o padrão de erro. É útil para imprimir mensagens de erro em tempo de execução, bem como para depurar e identificar problemas em seu código.

O padrão de erro é geralmente redirecionado para o mesmo local que o padrão de saída, mas você também pode redirecioná-lo para uma localização diferente. Isso pode ser útil para separar a saída do programa e os erros em arquivos diferentes.

Lembre-se de que, ao escrever para o padrão de erro, você deve usar a função `cerr` e não `cout`, que é usada para a saída padrão. Além disso, é importante adicionar `endl` ao final da mensagem para que o sistema a registre corretamente.

## Veja também

- [Documentação do cerr na CPlusPlus.com](https://www.cplusplus.com/reference/ios/ios_base/cerr)
- [O que é o padrão de erro em C++?](https://pt.stackoverflow.com/questions/112377/o-que-%C3%A9-o-padr%C3%A3o-de-erro-em-c)
- [Tratando exceções e erros em C++](https://docs.microsoft.com/pt-br/cpp/cpp/error-handling-in-cpp)