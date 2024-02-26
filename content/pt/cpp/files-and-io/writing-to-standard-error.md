---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:37.556752-07:00
description: "Escrever para o erro padr\xE3o (`stderr`) em C++ envolve a sa\xEDda\
  \ de mensagens de erro ou diagn\xF3sticos que s\xE3o separados da sa\xEDda principal\
  \ do programa. Os\u2026"
lastmod: '2024-02-25T18:49:44.513320-07:00'
model: gpt-4-0125-preview
summary: "Escrever para o erro padr\xE3o (`stderr`) em C++ envolve a sa\xEDda de mensagens\
  \ de erro ou diagn\xF3sticos que s\xE3o separados da sa\xEDda principal do programa.\
  \ Os\u2026"
title: "Escrevendo para o erro padr\xE3o"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Escrever para o erro padrão (`stderr`) em C++ envolve a saída de mensagens de erro ou diagnósticos que são separados da saída principal do programa. Os programadores fazem isso para direcionar os erros para um fluxo diferente, permitindo uma depuração e um tratamento de erros mais fáceis, distinguindo a saída normal das mensagens de erro.

## Como fazer:

Em C++, escrever para o erro padrão pode ser alcançado usando o fluxo `cerr`, que faz parte da biblioteca padrão. Aqui está um exemplo básico:

```cpp
#include <iostream>

int main() {
    // Escrevendo para a saída padrão
    std::cout << "Esta é uma mensagem normal." << std::endl;
    
    // Escrevendo para o erro padrão
    std::cerr << "Esta é uma mensagem de erro." << std::endl;
    
    return 0;
}
```

Saída de Amostra:
```
Esta é uma mensagem normal.
Esta é uma mensagem de erro.
```

Neste caso, ambas as mensagens normalmente aparecerão no seu terminal, mas você pode redirecioná-las separadamente em um shell. Por exemplo, você pode enviar a saída padrão para um arquivo enquanto permite que os erros sejam exibidos na tela.

Para um registro e tratamento de erros mais avançados, bibliotecas de terceiros como `spdlog` ou `boost.log` podem ser empregadas. Essas bibliotecas oferecem recursos aprimorados para registro, incluindo formatação, níveis de log e saída de arquivo.

Aqui está como você pode usar o `spdlog` para escrever uma mensagem de erro:

```cpp
#include "spdlog/spdlog.h"

int main() {
    // Inicializando o spdlog
    spdlog::info("Esta é uma mensagem normal.");
    spdlog::error("Esta é uma mensagem de erro.");
    
    return 0;
}
```

Nota: Para usar o `spdlog`, você precisa adicioná-lo ao seu projeto. Você pode fazer isso clonando o repositório do GitHub ou usando um gerenciador de pacotes como `vcpkg` ou `conan`.

Lembre-se, a escolha entre usar fluxos padrão diretamente ou uma biblioteca como `spdlog` depende da complexidade da sua aplicação e das suas necessidades específicas em relação ao tratamento e registro de erros.
