---
title:                "Usando um shell interativo (REPL)"
date:                  2024-01-26T04:11:47.086661-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando um shell interativo (REPL)"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Um shell interativo, ou Loop de Leitura-Avaliação-Impressão (REPL, em inglês), é uma ferramenta que fornece um ambiente de codificação em tempo real para testar trechos de código instantaneamente. Os programadores o utilizam para obter feedback rápido durante o desenvolvimento, aprendizado e depuração.

## Como Fazer:
C não vem com um REPL integrado, mas você pode usar ferramentas de terceiros. Aqui está um vislumbre usando o Cling, um interpretador de C++ que também pode lidar com código C:

```C
#include <stdio.h>

int main() {
    printf("Olá, mundo REPL!\n");
    return 0;
}
```

Saída no Cling REPL:
```
[cling]$ .x yourscript.c
Olá, mundo REPL!
```

O Cling executa o script e imprime a saída instantaneamente.

## Aprofundamento
REPLs são padrão em linguagens dinâmicas como Python ou Ruby, mas para linguagens compiladas como C, eles são menos comuns. Historicamente, o ciclo compilar-executar-depurar não se prestava à exploração interativa. Ferramentas como o Cling e compiladores C online oferecem experiências semelhantes ao REPL, envolvendo seu código C em um ambiente C++.

Alternativas ao Cling incluem interpretadores C como CINT e Ch. Essas ferramentas permitem iteração rápida, mas podem não ser adequadas para todos os cenários de desenvolvimento devido a restrições de desempenho e suporte para recursos complexos.

A implementação de um REPL em uma linguagem compilada envolve compilar e executar trechos de código instantaneamente, o que não é trivial e pode ter limitações em comparação com as capacidades completas da linguagem.

## Veja Também
- Cling: https://github.com/root-project/cling
- Compilador C Online e REPL: https://repl.it/languages/c
- CINT: http://root.cern.ch/drupal/content/cint
- Interpretador Ch: http://www.softintegration.com/products/chstandard/
