---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:13.593397-07:00
description: "Como Fazer: C++ oferece v\xE1rias maneiras de escrever em um arquivo\
  \ de texto, mas um dos m\xE9todos mais diretos \xE9 usando a biblioteca `<fstream>`,\
  \ que\u2026"
lastmod: '2024-03-13T22:44:46.897490-06:00'
model: gpt-4-0125-preview
summary: "C++ oferece v\xE1rias maneiras de escrever em um arquivo de texto, mas um\
  \ dos m\xE9todos mais diretos \xE9 usando a biblioteca `<fstream>`, que proporciona\
  \ a classe `ofstream` (fluxo de arquivo de sa\xEDda) projetada para opera\xE7\xF5\
  es de escrita em arquivo."
title: Escrevendo um arquivo de texto
weight: 24
---

## Como Fazer:
C++ oferece várias maneiras de escrever em um arquivo de texto, mas um dos métodos mais diretos é usando a biblioteca `<fstream>`, que proporciona a classe `ofstream` (fluxo de arquivo de saída) projetada para operações de escrita em arquivo.

### Exemplo usando `<fstream>`:
```cpp
#include <fstream>
#include <iostream>

int main() {
    std::ofstream file("example.txt");
    if (file.is_open()) {
        file << "Olá, mundo!\n";
        file << "Escrever em um arquivo em C++ é simples.";
        file.close();
    } else {
        std::cerr << "Falha ao abrir o arquivo\n";
    }
    return 0;
}
```

**Saída de amostra em 'example.txt':**
```
Olá, mundo!
Escrever em um arquivo em C++ é simples.
```

Ao lidar com dados mais complexos ou precisando de mais controle sobre o processo de escrita, programadores podem recorrer a bibliotecas de terceiros, como o Boost Filesystem.

### Exemplo usando Boost Filesystem:
Para usar o Boost para operações de arquivo, você primeiro precisará instalar as bibliotecas Boost. O exemplo a seguir demonstra a criação e escrita em um arquivo usando `boost::filesystem` e `boost::iostreams`.

```cpp
#include <boost/filesystem.hpp>
#include <boost/iostreams/device/file.hpp>
#include <boost/iostreams/stream.hpp>
#include <iostream>

namespace io = boost::iostreams;
namespace fs = boost::filesystem;

int main() {
    fs::path filePath("boost_example.txt");
    io::stream_buffer<io::file_sink> buf(filePath.string());
    std::ostream out(&buf);
    out << "Boost torna as operações de arquivo fáceis.\n";
    out << "Esta é uma linha escrita com o Boost.";
    
    return 0;
}
```

**Saída de amostra em 'boost_example.txt':**
```
Boost torna as operações de arquivo fáceis.
Esta é uma linha escrita com o Boost.
```

A escolha entre o C++ puro e uma biblioteca de terceiros como o Boost pode depender dos requisitos específicos do seu projeto e de quanto controle ou flexibilidade você precisa sobre as operações de E/S de arquivo.
