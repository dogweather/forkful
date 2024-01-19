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

---

# Verificar se um Diretório Existe em C++

---

## O que é e Porquê?

Verificar se um diretório existe em C++ é uma simples condição que controla o fluxo do programa. Os programadores fazem isso para evitar erros de entrada/saída, que podem ocorrer se o diretório não existir quando o programa tenta acessá-lo.

---

## Como fazer:

O seguinte bloco de código C++ detalha como pode verificar a existência de um diretório.

```C++
#include <filesystem>

bool diretório_existe(const std::string& caminho_do_diretório){
    return std::filesystem::exists(caminho_do_diretório);
}
```

Este código define uma função que recebe um caminho do diretório como um argumento, e depois usa a função `exists()` da biblioteca `filesystem` para verificar se o diretório existe ou não.

Por exemplo, aqui está um exemplo de chamada e saída da função.

```C++
std::cout << diretório_existe("/Users/seunome") << std::endl;
```

Saída: `1` para diretorio existente e `0` para um diretorio não existente.


---

## Análise Profunda

Com a introdução do C++17, a biblioteca `filesystem` foi adicionada, que fornece as funções para realizar as operações de sistema de arquivos. Antes do C++17, nós precisávamos de chamadas ao sistema operacional por meio de `dirent.h` ou `sys/stat.h` em Unix/Linux ou `windows.h` em Windows.

Alternativamente, frameworks como Boost ou QT fornecem suas próprias implementações multi-plataforma para verificação de existência de diretório. 

Vale ressaltar que a função `exists()` retorna `false` não apenas quando o diretório não existe, mas também se ocorrer um erro enquanto acessa o caminho especificado. Portanto, pode ser necessário lidar adequadamente com as exceções ao usar esta função em um ambiente de produção.

---

## Veja Também

- Biblioteca Filesystem C++ (https://en.cppreference.com/w/cpp/filesystem)

- Funcionalidades de C++17 (https://isocpp.org/wiki/faq/cpp17-language)

- Documentação do Boost Filesystem Library (https://www.boost.org/doc/libs/1_76_0/libs/filesystem/doc/index.htm)

- Guia QT para lidar com Arquivos e Diretórios (https://doc.qt.io/qt-5/qdir.html)