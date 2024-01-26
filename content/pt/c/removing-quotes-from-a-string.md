---
title:                "Removendo aspas de uma string"
date:                  2024-01-26T03:38:01.567242-07:00
model:                 gpt-4-0125-preview
simple_title:         "Removendo aspas de uma string"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Remover aspas de uma string significa eliminar qualquer marca de aspas — seja simples ('') ou dupla ("") — que faça parte do conteúdo da string. Programadores fazem isso para higienizar a entrada, preparar dados para processamento adicional ou evitar erros de sintaxe ao lidar com caminhos de arquivos e comandos em linguagens que usam aspas para demarcar strings.

## Como Fazer:

Aqui está uma função em C que eliminará essas indesejadas aspas das suas strings:

```c
#include <stdio.h>
#include <string.h>

void remove_quotes(char *str) {
    char *p_read = str, *p_write = str;
    while (*p_read) {
        if (*p_read != '"' && *p_read != '\'') {
            *p_write++ = *p_read;
        }
        p_read++;
    }
    *p_write = '\0';
}

int main() {
    char str[] = "He said, \"Hello, 'world'!\"";
    printf("Original: %s\n", str);
    remove_quotes(str);
    printf("Saneado: %s\n", str);
    return 0;
}
```

Exemplo de saída:

```
Original: He said, "Hello, 'world'!"
Saneado: He said, Hello, world!
```

## Aprofundamento

Remover aspas de uma string é uma tarefa desde os primórdios da programação, onde a higiene de dados era e ainda é fundamental para evitar erros (como ataques de injeção de SQL) ou garantir que uma string possa ser passada com segurança para sistemas que podem confundir uma aspa com um caractere de controle.

Historicamente, diferentes linguagens lidam com essa tarefa de maneiras distintas — algumas têm funções embutidas (como `strip` em Python), enquanto outras, como C, exigem implementação manual devido ao seu foco em dar aos desenvolvedores controle de baixo nível.

Alternativas incluem usar funções de biblioteca como `strpbrk` para encontrar aspas ou empregar expressões regulares (com bibliotecas como PCRE) para padrões mais complexos, embora isso possa ser excessivo apenas para remover aspas.

A implementação acima simplesmente verifica cada caractere na string, copiando somente caracteres não-aspas para a localização do ponteiro de escrita. Isso é eficiente porque é feito no local, sem necessidade de memória extra para a string resultante.

## Veja Também

- [Funções da Biblioteca Padrão C](http://www.cplusplus.com/reference/clibrary/)
- [PCRE - Expressões Regulares Compatíveis com Perl](https://www.pcre.org/)
- [Entendendo Ponteiros em C](https://www.learn-c.org/en/Pointers)
- [Programação Segura em C](https://owasp.org/www-project-secure-coding-in-c)