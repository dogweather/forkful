---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:53.274473-07:00
description: "Como fazer: No C++ moderno (C++17 e al\xE9m), voc\xEA pode usar a biblioteca\
  \ filesystem para verificar se um diret\xF3rio existe. Ela fornece uma maneira direta\
  \ e\u2026"
lastmod: '2024-03-13T22:44:46.893565-06:00'
model: gpt-4-0125-preview
summary: "No C++ moderno (C++17 e al\xE9m), voc\xEA pode usar a biblioteca filesystem\
  \ para verificar se um diret\xF3rio existe."
title: "Verificando se um diret\xF3rio existe"
weight: 20
---

## Como fazer:
No C++ moderno (C++17 e além), você pode usar a biblioteca filesystem para verificar se um diretório existe. Ela fornece uma maneira direta e padronizada de realizar operações no sistema de arquivos, incluindo a verificação da existência de um diretório.

```cpp
#include <iostream>
#include <filesystem>

namespace fs = std::filesystem;

int main() {
    const fs::path dirPath = "/caminho/para/diretorio";

    if (fs::exists(dirPath) && fs::is_directory(dirPath)) {
        std::cout << "O diretório existe." << std::endl;
    } else {
        std::cout << "O diretório não existe." << std::endl;
    }

    return 0;
}
```
Saída de exemplo se o diretório existir:
```
O diretório existe.
```

Saída de exemplo se o diretório não existir:
```
O diretório não existe.
```

Para projetos que ainda não estão usando C++17 ou para recursos adicionais, a biblioteca Boost Filesystem é uma escolha de terceiros popular que oferece funcionalidade similar.

```cpp
#include <iostream>
#include <boost/filesystem.hpp>

namespace fs = boost::filesystem;

int main() {
    const fs::path dirPath = "/caminho/para/diretorio";

    if (fs::exists(dirPath) && fs::is_directory(dirPath)) {
        std::cout << "O diretório existe." << std::endl;
    } else {
        std::cout << "O diretório não existe." << std::endl;
    }

    return 0;
}
```
Usando Boost Filesystem, a saída seria idêntica ao exemplo do filesystem C++17, dependendo da existência do diretório no caminho especificado.
