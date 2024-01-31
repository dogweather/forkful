---
title:                "Escrevendo no erro padrão"
date:                  2024-01-19
simple_title:         "Escrevendo no erro padrão"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## O Que é & Por Que?

Escrever no erro padrão (stderr) permite que mensagens de erro e diagnósticos sejam separados da saída padrão (stdout). Programadores usam isso para facilitar a depuração e permitir que usuários roteiem mensagens de erro de forma diferente da saída regular.

## Como Fazer:

Você pode escrever no stderr em C++ usando o objeto `cerr` da biblioteca iostream. Veja como:

```C++
#include <iostream>

int main() {
    std::cout << "Esta é a saída padrão." << std::endl;
    std::cerr << "Isto é uma mensagem de erro." << std::endl;
    return 0;
}
```

Saída esperada:
```
Esta é a saída padrão.
Isto é uma mensagem de erro.
```

## Mergulho Profundo:

Historicamente, a distinção entre stdout e stderr vem dos tempos dos primeiros terminais Unix, onde era útil separar as saídas. Alternativas incluem escrever em arquivos de log ou usar bibliotecas de log customizadas. Internamente, stderr é um stream desbufferizado, isso significa que as mensagens são enviadas imediatamente r para o destino de saída.

## Veja Também:

- Documentação do C++ para `<iostream>`: http://www.cplusplus.com/reference/iostream/
- Tutorial do C++ sobre manipulação de arquivos e streams: http://www.cplusplus.com/doc/tutorial/files/
- A especificação do C++ atualizada: https://isocpp.org/std/the-standard
