---
date: 2024-01-20 17:55:35.705173-07:00
description: "Como Fazer: Os argumentos da linha de comando s\xE3o uma pr\xE1tica\
  \ antiga, remontando aos primeiros dias dos sistemas operacionais tipo Unix. Uma\
  \ alternativa \xE9\u2026"
lastmod: '2024-04-05T21:53:47.245684-06:00'
model: gpt-4-1106-preview
summary: "Os argumentos da linha de comando s\xE3o uma pr\xE1tica antiga, remontando\
  \ aos primeiros dias dos sistemas operacionais tipo Unix."
title: Lendo argumentos da linha de comando
weight: 23
---

## Como Fazer:
```C++
#include <iostream>

int main(int argc, char *argv[]) {
    std::cout << "Você passou " << argc - 1 << " argumentos:\n";
    for (int i = 1; i < argc; ++i) {
        std::cout << i << ": " << argv[i] << '\n';
    }
    return 0;
}

/*
Compila o código e o executa com alguns argumentos:
> g++ programa.cpp -o programa && ./programa arg1 arg2 arg3

Saída esperada:
Você passou 3 argumentos:
1: arg1
2: arg2
3: arg3
*/
```

## Mergulho Profundo
Os argumentos da linha de comando são uma prática antiga, remontando aos primeiros dias dos sistemas operacionais tipo Unix. Uma alternativa é usar arquivos de configuração ou variáveis de ambiente, mas isso pode ser mais complexo e menos direto. A função `main` possui dois parâmetros opcionais: `argc` (argument count) conta quantos argumentos foram passados, e `argv` (argument vector) é um array dos argumentos propriamente ditos. O primeiro argumento `argv[0]` é, por convenção, o nome do programa.

## Veja Também
- Documentação C++ sobre a função `main`: https://en.cppreference.com/w/cpp/language/main_function
- Tutorial sobre argumentos da linha de comando: https://www.cplusplus.com/articles/DEN36Up4/
- Guia para argumentos de linha de comando POSIX: http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap12.html
