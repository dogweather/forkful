---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:57:18.982971-07:00
description: "Como Fazer: Em C, o cabe\xE7alho `<time.h>` fornece as fun\xE7\xF5es\
  \ e tipos necess\xE1rios para trabalhar com datas e hor\xE1rios. A fun\xE7\xE3o\
  \ `time()` recupera o hor\xE1rio\u2026"
lastmod: '2024-03-13T22:44:47.061494-06:00'
model: gpt-4-0125-preview
summary: "Em C, o cabe\xE7alho `<time.h>` fornece as fun\xE7\xF5es e tipos necess\xE1\
  rios para trabalhar com datas e hor\xE1rios."
title: Obtendo a data atual
weight: 29
---

## Como Fazer:
Em C, o cabeçalho `<time.h>` fornece as funções e tipos necessários para trabalhar com datas e horários. A função `time()` recupera o horário atual, enquanto `localtime()` converte esse horário para o fuso horário local. Para exibir a data, usamos `strftime()` para formatá-la como uma string.

Aqui está um exemplo básico:

```c
#include <stdio.h>
#include <time.h>

int main() {
    char buffer[80];
    time_t rawtime;
    struct tm *timeinfo;

    // Pegar o horário atual
    time(&rawtime);
    // Converter para o horário local
    timeinfo = localtime(&rawtime);
    
    // Formatar a data e imprimir
    strftime(buffer, 80, "A data de hoje é %Y-%m-%d", timeinfo);
    printf("%s\n", buffer);

    return 0;
}
```

Uma saída do exemplo pode ser assim:

```
A data de hoje é 2023-04-12
```

## Aprofundamento
O tratamento do tempo em C, como facilitado por `<time.h>`, remonta aos primeiros dias da linguagem e dos sistemas UNIX. Ele é construído em torno do tipo de dado `time_t`, que representa o horário atual como o número de segundos desde a Epoch Unix (1 de Janeiro de 1970). Embora isso seja eficiente e universalmente compatível, também significa que as funções de tempo da biblioteca padrão do C são inerentemente limitadas pela gama e resolução do `time_t`.

Aplicações modernas, especialmente aquelas que requerem carimbos de tempo de alta resolução ou lidam com datas muito no futuro ou no passado, podem achar essas limitações desafiadoras. Por exemplo, o problema do Ano 2038 é uma ilustração famosa onde sistemas que usam um `time_t` de 32 bits sofrerão overflow.

Para um tratamento de tempo e data mais complexo, muitos programadores recorrem a bibliotecas externas ou às funcionalidades fornecidas pelo sistema operacional. Em C++, por exemplo, a biblioteca `<chrono>` oferece capacidades de manipulação de tempo mais precisas e versáteis.

Apesar de suas limitações, a simplicidade e ubiquidade das funções de tempo do C as tornam perfeitamente adequadas para muitas aplicações. Entender essas ferramentas é fundamental para programadores de C, oferecendo uma mistura de contexto histórico de programação e utilidade prática no dia a dia.
