---
title:                "Escrevendo para o erro padrão"
html_title:           "C++: Escrevendo para o erro padrão"
simple_title:         "Escrevendo para o erro padrão"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## O que e por que usar a saída de erro padrão no programa?

A saída de erro padrão é uma maneira pela qual os programadores podem exibir mensagens de erro ou informações importantes em seus programas. Diferente da saída normal que é exibida ao usuário, a saída de erro é impressa na tela de erros do sistema. Isso é útil para que os usuários e os desenvolvedores possam identificar e corrigir erros em seus programas.

## Como fazer:

Para imprimir mensagens na saída de erro padrão em C++, você pode usar a função `std::cerr` e o operador `<<`, como mostrado no exemplo a seguir:

```C++
#include <iostream>

int main() {
  std::cerr << "Este é um erro de exemplo." << std::endl;
  return 0;
}
```

A saída deste programa seria:

```
Este é um erro de exemplo.
```

## Mergulho profundo:

A ideia de utilizar a saída de erro padrão é antiga e vem desde os primeiros dias da programação. O objetivo principal é fornecer uma maneira de informar os usuários sobre erros e problemas que ocorrem durante a execução do programa. No entanto, com o avanço da tecnologia, surgiram novas maneiras de lidar com erros, como o uso de exceções em C++.

No entanto, a saída de erro padrão continua sendo uma ferramenta importante para os programadores. Além de exibir mensagens de erro, também pode ser usada para imprimir informações de diagnóstico durante o desenvolvimento de um programa, como valores de variáveis e fluxo de execução.

A implementação desta funcionalidade pode variar de acordo com o sistema operacional e a linguagem de programação utilizada. Em geral, a saída de erro padrão é redirecionada para um dispositivo específico, como o console ou arquivo de log.

## Veja também:

Para mais informações sobre a saída de erro padrão e como utilizá-la em seus programas em C++, você pode consultar as seguintes fontes:

- Documentação oficial do C++ sobre a função `std::cerr`: https://en.cppreference.com/w/cpp/io/cerr
- Tutorial sobre a saída de erro padrão em C++: https://www.learncpp.com/cpp-tutorial/186-basic-use-of-cerr/
- Como depurar programas em C++ utilizando a saída de erro padrão: https://www.geeksforgeeks.org/c-debugging-practise-using-stderr/