---
title:                "Escrevendo no erro padrão"
html_title:           "C++: Escrevendo no erro padrão"
simple_title:         "Escrevendo no erro padrão"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que

Escrever para a saída padrão de erro é útil para exibir mensagens de erro ou informação de depuração durante a execução de um programa.

## Como fazer

Para escrever para saída padrão de erro em C++, é necessário incluir a biblioteca `iostream` e usar o objeto `cerr`. Aqui está um exemplo simples:

```C++
#include <iostream>

using namespace std;

int main() {
  cerr << "Mensagem de erro" << endl;
  return 0;
}
```

Isso irá imprimir a string "Mensagem de erro" na saída de erro. É importante usar `endl` no final da mensagem para garantir que a mensagem seja exibida imediatamente.

Se você quiser adicionar informações de depuração, é possível usar a mesma sintaxe para imprimir valores de variáveis ​​ou mensagens de status. Por exemplo:

```C++
#include <iostream>

using namespace std;

int main() {
  int idade = 25;
  cerr << "A idade é: " << idade << endl;
  cerr << "O status é: " << "OK" << endl;
  return 0;
}
```

Isso irá imprimir "A idade é: 25" e "O status é: OK" na saída de erro.

## Mergulho Profundo

Escrever para a saída de erro pode ser útil durante a depuração de um programa, pois permite que você veja informações importantes sobre o que está acontecendo em tempo real. Além disso, se você estiver escrevendo um programa maior que interage com outros sistemas ou hardware, escrever para a saída de erro pode ser uma forma eficaz de registrar informações para investigar problemas mais tarde.

Também é importante lembrar que a saída de erro é diferente da saída de console padrão (`cout`). A saída de erro é usada especificamente para mensagens de erro ou depuração, enquanto a saída padrão é usada para resultados ou mensagens regulares.

## Veja também

- [Documentação da biblioteca iostream](http://www.cplusplus.com/reference/iostream/)
- [Tutorial sobre saída padrão em C++](https://www.learncpp.com/cpp-tutorial/standard-output-cout-and-cerr/)