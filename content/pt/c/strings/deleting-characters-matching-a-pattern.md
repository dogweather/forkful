---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:55:22.258942-07:00
description: "Como fazer: C n\xE3o possui uma fun\xE7\xE3o embutida para deletar diretamente\
  \ caracteres de uma string baseado em um padr\xE3o, ao contr\xE1rio de algumas linguagens\
  \ de\u2026"
lastmod: '2024-03-13T22:44:47.032161-06:00'
model: gpt-4-0125-preview
summary: "C n\xE3o possui uma fun\xE7\xE3o embutida para deletar diretamente caracteres\
  \ de uma string baseado em um padr\xE3o, ao contr\xE1rio de algumas linguagens de\
  \ n\xEDvel mais alto."
title: "Deletando caracteres que correspondem a um padr\xE3o"
weight: 5
---

## Como fazer:
C não possui uma função embutida para deletar diretamente caracteres de uma string baseado em um padrão, ao contrário de algumas linguagens de nível mais alto. No entanto, você pode facilmente realizar essa tarefa manualmente iterando sobre a string e construindo uma nova que exclua os caracteres indesejados. Por exemplo, vamos supor que você queira remover todos os dígitos de uma string. Você pode fazer isso da seguinte forma:

```c
#include <stdio.h>
#include <ctype.h>

void remove_digits(char *str) {
    char *src = str, *dst = str;
    while (*src) {
        if (!isdigit((unsigned char)*src)) {
            *dst++ = *src;
        }
        src++;
    }
    *dst = '\0';
}

int main() {
    char str[] = "C Programming 101: The Basics!";
    remove_digits(str);
    printf("Resultado: %s\n", str);
    return 0;
}
```

Saída de exemplo:
```
Resultado: C Programming : The Basics!
```

Este exemplo utiliza `isdigit` de `ctype.h` para identificar dígitos, deslocando caracteres não-dígitos para o início da string e terminando a string uma vez que todos os caracteres foram avaliados.

## Aprofundamento
A solução apresentada utiliza uma abordagem de dois ponteiros dentro do mesmo array para efetivamente filtrar caracteres indesejados, uma técnica emblemática da filosofia de gerenciamento de memória prático do C. Este método é eficiente porque opera in-place, evitando a necessidade de alocação de memória adicional e assim minimizando sobrecarga.

Historicamente, a ausência de funções de manipulação de string de alto nível em C forçou programadores a desenvolver um entendimento profundo do manuseio de strings no nível da memória, levando a abordagens inovadoras como a acima. Embora isso tenha a vantagem de maior controle e eficiência, vem com um risco maior de erros, como estouros de buffer e erros off-by-one.

Em contextos de desenvolvimento modernos, especialmente aqueles que enfatizam segurança e proteção, linguagens que abstraem tais operações de baixo nível podem ser preferidas para tarefas de manipulação de strings. No entanto, entender e utilizar essas técnicas de C continua sendo inestimável para cenários que demandam otimização de desempenho detalhada ou para trabalhar dentro de ambientes onde o minimalismo e a velocidade do C são primordiais.
