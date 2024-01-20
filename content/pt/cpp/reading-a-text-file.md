---
title:                "Lendo um arquivo de texto"
html_title:           "Bash: Lendo um arquivo de texto"
simple_title:         "Lendo um arquivo de texto"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Ler um arquivo de texto em C++ é um processo que permite ao programa acessar e manipular dados armazenados nesse arquivo. É uma habilidade crítica para os programadores, pois muitos programas dependem da leitura de informações a partir de arquivos.

## Como Fazer:

Aqui está uma maneira básica de ler um arquivo de texto em C++ usando a biblioteca `fstream`.

```C++
#include <fstream>
#include <iostream>
#include <string>

int main() {
    std::ifstream file("arquivo.txt");
    std::string line;
    
    if (file.is_open()) {
        while (std::getline(file, line)) {
            std::cout << line << '\n';
        }
        file.close();
    } else {
        std::cout << "Não foi possível abrir o arquivo.\n";
    }

    return 0;
}
```

Este programa lê cada linha do arquivo "arquivo.txt" e as imprime no console. Se o arquivo não puder ser aberto, ele irá exibir a mensagem "Não foi possível abrir o arquivo.".

## Mais a Fundo

A leitura de arquivos de texto em C++ tem uma longa história, e existem várias maneiras de fazer isso, cada uma com seus próprios prós e contras. Neste artigo, usamos a biblioteca `fstream` por seu suporte extensivo e facilidade de uso.

Existem alternativas à `fstream`, tal como a biblioteca chamada `cstdio`. Essa biblioteca pertence ao C, que é a linguagem de programação que originou o C++. Contudo, `fstream` geralmente é preferida por seu estilo orientado a objetos.

Quando se lê um arquivo de texto em C++, o arquivo é lido e os dados são armazenados na memória do computador, linha por linha. Cada linha é lida como uma string, o que torna mais fácil para o programador manipular os dados.

## Veja Também

Artigos relacionados que você pode achar útil:

- Mais sobre a biblioteca `fstream`: [cplusplus.com | fstream](http://www.cplusplus.com/reference/fstream/)
- Leitura de arquivos com a biblioteca `cstdio`: [cplusplus.com | cstdio](http://www.cplusplus.com/reference/cstdio/)

Não existe uma única "maneira correta" de ler um arquivo de texto em C++. A melhor abordagem depende das necessidades específicas do seu programa.