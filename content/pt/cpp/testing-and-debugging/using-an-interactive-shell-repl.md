---
date: 2024-01-26 04:12:06.076563-07:00
description: "Um REPL (Read-Eval-Print-Loop - Ler-Avaliar-Imprimir-Loop) \xE9 um ambiente\
  \ de programa\xE7\xE3o simples e interativo. Programadores o utilizam para experimenta\xE7\
  \xE3o\u2026"
lastmod: '2024-03-13T22:44:46.881188-06:00'
model: gpt-4-0125-preview
summary: "Um REPL (Read-Eval-Print-Loop - Ler-Avaliar-Imprimir-Loop) \xE9 um ambiente\
  \ de programa\xE7\xE3o simples e interativo. Programadores o utilizam para experimenta\xE7\
  \xE3o\u2026"
title: Usando um shell interativo (REPL)
---

{{< edit_this_page >}}

## O Quê & Por Quê?
Um REPL (Read-Eval-Print-Loop - Ler-Avaliar-Imprimir-Loop) é um ambiente de programação simples e interativo. Programadores o utilizam para experimentação da linguagem em tempo real, tarefas rápidas, ou para entender novos conceitos sem a sobrecarga de criar aplicações completas.

## Como Fazer:
C++ não vem com um REPL integrado, mas ferramentas como Cling oferecem essa capacidade. Aqui está como usar o Cling para calcular a soma de dois números:

```C++
#include <iostream>

int main() {
    int a = 5;
    int b = 7;
    std::cout << "A soma é: " << a + b << std::endl;
    return 0;
}

// Saída:
// A soma é: 12
```

Inicie o Cling e insira o código linha por linha, observando a saída após cada comando. É um feedback imediato, sem compilação.

## Aprofundando
REPLs são comuns para linguagens como Python ou Lisp, e existem desde os anos 1960. Para C++, uma linguagem compilada, o conceito não se encaixa tão naturalmente, razão pela qual ferramentas como Cling existem - elas interpretam C++ em tempo real. Alternativas incluem compiladores online ou programas de teste de pequena escala compilados tradicionalmente. Cling é construído em cima de LLVM e Clang, proporcionando uma ponte para que C++ seja usado de forma interpretada.

## Veja Também
- [Cling](https://root.cern/cling/): Um interpretador interativo de C++, construído em cima das bibliotecas LLVM e Clang.
- [Cadernos Jupyter](https://jupyter.org/): Oferece um shell interativo dentro de um ambiente de caderno, suporta C++ por meio do kernel xeus-cling.
- [LLVM](https://llvm.org/): Uma coleção de tecnologias compiladoras modulares e reutilizáveis, nas quais Cling é baseado.
