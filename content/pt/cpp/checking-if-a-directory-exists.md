---
title:                "Verificando se um diretório existe"
html_title:           "C++: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que

Você pode estar se perguntando por que é necessário verificar se um diretório existe em seu programa em C++. Existem várias razões pelas quais isso pode ser útil, como garantir que seu programa esteja lidando corretamente com arquivos e diretórios, ou permitindo que o usuário especifique seu próprio diretório de arquivos.

## Como Fazer

Existem várias maneiras de verificar se um diretório existe em C++. Aqui estão duas abordagens possíveis, utilizando as funções `opendir()` e `filesystem::path::exists()`.

Primeiro, vamos ver como fazer isso usando a função `opendir()`:

```C++
#include <iostream>
#include <dirent.h>

int main() {
    // especificando o diretório a ser verificado
    const char* directory = "./meu_diretorio";
    
    // abrindo o diretório
    DIR* dir = opendir(directory);

    // verificando se o diretório foi aberto com sucesso
    if(dir) {
        std::cout << "O diretório existe." << std::endl;
        // faz algo se o diretório existe
        // ...
        
        // fechando o diretório
        closedir(dir);
    } else {
        std::cout << "O diretório não existe." << std::endl;
    }
   
    return 0;
}
```

A saída deste código será "O diretório não existe.", pois não criamos o diretório "meu_diretorio" para fins de demonstração. Mas se você alterar o valor da variável `directory` para um diretório existente em seu sistema, como por exemplo "C:\Arquivos", a saída será "O diretório existe."

Agora, vamos ver como fazer isso usando a função `filesystem::path::exists()`:

```C++
#include <iostream>
#include <filesystem>

namespace fs = std::filesystem;

int main() {
    // especificando o diretório a ser verificado
    fs::path directory = "./meu_diretorio";
    
    // verificando se o diretório existe
    if(fs::exists(directory)) {
        std::cout << "O diretório existe." << std::endl;
        // faz algo se o diretório existe
        // ...
    } else {
        std::cout << "O diretório não existe." << std::endl;
    }
   
    return 0;
}
```

A lógica aqui é a mesma, mas agora estamos usando a biblioteca `filesystem` do C++17. Mais uma vez, a saída será "O diretório não existe." se o diretório especificado não existir, ou "O diretório existe." se o diretório existir.

## Detalhes Avançados

Se você quiser entender mais sobre como essas funções funcionam, aqui estão algumas informações adicionais.

A função `opendir()` é definida na biblioteca `<dirent.h>` e é usada para abrir um diretório e retornar um ponteiro para uma estrutura `DIR`. Se essa função falhar, ela retornará `NULL`.

Já a função `filesystem::exists()` é definida na biblioteca `<filesystem>` e é usada para verificar se um caminho existente no sistema de arquivos existe. Ela retorna `true` se o caminho existir, `false` caso contrário.

## Veja Também

- [Gerenciamento de Arquivos em C++](https://pt.wikipedia.org/wiki/Gerenciamento_de_arquivos_em_C%2B%2B)
- [Documentação da Função `opendir()`](https://en.cppreference.com/w/cpp/io/c/opendir)
- [Documentação da Função `filesystem::exists()`](https://en.cppreference.com/w/cpp/filesystem/exists)