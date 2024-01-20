---
title:                "Criando um arquivo temporário"
html_title:           "Bash: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Crie um arquivo temporário em C++: Tutorial para Bolivaristas 
## O Que & Por Quê?
Criar um arquivo temporário em C++ é um processo no qual um arquivo é gerado para armazenar dados temporariamente. Programadores frequentemente fazem isso quando lidam com grandes volumes de dados que não são necessários a longo prazo.

## Como Fazer:
Aqui está um exemplo de como criar um arquivo temporário em C++ usando a biblioteca `<cstdio>`:

```C++
#include <cstdio>

int main()
{
    char tempPath[L_tmpnam];
    std::tmpnam(tempPath);

    printf("Created temporary file at: %s\n", tempPath);

    return 0;
}
```
Este código criará um arquivo temporário único em seu sistema e imprimirá o caminho para ele.

## Mergulho Profundo:
- **Contexto histórico**: O sistema de arquivos temporários foi introduzido em C++ para permitir o manuseio de grandes volumes de dados que os sistemas de memória não conseguem gerenciar eficientemente. É uma prática comum em muitas operações de programação, como ordenação, busca e computação gráfica.
  
- **Alternativas**: Outras alternativas para a criação de um arquivo temporário em C++ podem incluir o uso de bibliotecas externas, como a Boost Filesystem, que fornece uma interface orientada a objetos para manipulação de arquivos e diretorios.

- **Detalhes de implementação**: A função `std::tmpnam` em `<cstdio>` gera nomes únicos para arquivos temporários. Isso evita conflitos entre diferentes arquivos temporários no mesmo sistema.

## Veja Também:
1. Documentação da biblioteca C++ `<cstdio>`: [http://www.cplusplus.com/reference/cstdio/](http://www.cplusplus.com/reference/cstdio/)
2. Precauções ao trabalhar com arquivos temporários: [https://en.cppreference.com/w/cpp/io/c/tmpnam](https://en.cppreference.com/w/cpp/io/c/tmpnam)
3. Biblioteca Boost Filesystem: [https://www.boost.org/doc/libs/1_75_0/libs/filesystem/doc/index.htm](https://www.boost.org/doc/libs/1_75_0/libs/filesystem/doc/index.htm)