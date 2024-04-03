---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:56:31.837424-07:00
description: "Como Fazer: Em C, a fun\xE7\xE3o da biblioteca padr\xE3o `strlen()`\
  \ \xE9 comumente usada para encontrar o comprimento de uma string. Aqui est\xE1\
  \ um exemplo r\xE1pido."
lastmod: '2024-03-13T22:44:47.039660-06:00'
model: gpt-4-0125-preview
summary: "Em C, a fun\xE7\xE3o da biblioteca padr\xE3o `strlen()` \xE9 comumente usada\
  \ para encontrar o comprimento de uma string."
title: Encontrando o comprimento de uma string
weight: 7
---

## Como Fazer:
Em C, a função da biblioteca padrão `strlen()` é comumente usada para encontrar o comprimento de uma string. Aqui está um exemplo rápido:

```c
#include <stdio.h>
#include <string.h>

int main() {
    char minhaString[] = "Olá, Mundo!";
    size_t comprimento = strlen(minhaString);
    
    printf("Comprimento de '%s' é %zu.\n", minhaString, comprimento);
    
    return 0;
}
```

**Saída do Exemplo:**
```
Comprimento de 'Olá, Mundo!' é 13.
```

Neste exemplo, `strlen()` recebe uma string (`minhaString`) como entrada e retorna seu comprimento excluindo o terminador nulo. O uso de `size_t` para a variável de comprimento é recomendado porque é um tipo de inteiro sem sinal, capaz de representar o tamanho do maior objeto possível no sistema.

## Aprofundando:
A função `strlen()` faz parte da biblioteca padrão C desde a criação da linguagem. Por baixo dos panos, ela funciona incrementando um contador à medida que percorre a string até atingir o terminador nulo. No entanto, essa simplicidade vem com considerações de desempenho: porque `strlen()` conta os caracteres em tempo de execução, chamá-la repetidamente na mesma string em um loop, por exemplo, é ineficiente.

Em termos de segurança, `strlen()` e outras funções de manipulação de strings em C não verificam inerentemente estouros de buffer, tornando a programação cuidadosa essencial para evitar vulnerabilidades. Alternativas modernas em outras linguagens, tais como tipos de string que incluem o comprimento ou usam o manuseio seguro de buffer por padrão, eliminam alguns desses riscos e ineficiências.

Apesar de suas limitações, entender `strlen()` e o manuseio manual de strings em C é crucial para programadores, especialmente ao trabalhar com código de baixo nível ou quando o controle de desempenho e memória são primordiais. Isso também oferece insights valiosos sobre o funcionamento de abstrações de strings de alto nível em outras linguagens.
