---
title:                "Verificando se um diretório existe"
date:                  2024-01-19
simple_title:         "Verificando se um diretório existe"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Verificar se um diretório existe é simplesmente checar se um determinado caminho no sistema de arquivos aponta para um lugar que realmente está lá. Programadores fazem isso para evitar erros ao tentar acessar, ler, ou escrever em diretórios que não existem, prevenindo assim falhas e comportamentos indesejados em seus programas.

## Como Fazer:

A biblioteca `filesystem` em C++ faz esse trabalho. Veja como usar:

```C++
#include <iostream>
#include <filesystem>

int main() {
    std::filesystem::path dir = "algum_diretorio/";

    if (std::filesystem::exists(dir)) {
        std::cout << "O diretório existe!" << std::endl;
    } else {
        std::cout << "O diretório não existe." << std::endl;
    }

    return 0;
}
```
Se o diretório `algum_diretorio/` existir, a saída será:

```
O diretório existe!
```
Caso contrário:

```
O diretório não existe.
```

## Aprofundamento

Antes do C++17, os programadores usavam bibliotecas de terceiros como o Boost ou sys/stat.h para essa tarefa. A introdução do módulo `filesystem` no C++17 padronizou as operações de sistema de arquivos. Além de `std::filesystem::exists`, você pode usar funções como `is_directory` para verificar especificamente por diretórios, ou `create_directory` para criar um diretório se ele não existir. Sob o capô, `exists` faz chamadas de sistema específicas para cada plataforma, como o `stat` em Unix ou `GetFileAttributes` no Windows.

## Veja Também

Para mais detalhes sobre o módulo filesystem, consulte:

- [Documentação do std::filesystem](https://en.cppreference.com/w/cpp/filesystem)

E para entender erros comuns e exceções ao usar filesystem:

- [Exceções no std::filesystem](https://en.cppreference.com/w/cpp/filesystem/exists#Exceptions)
