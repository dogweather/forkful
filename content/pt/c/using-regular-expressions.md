---
title:                "Utilizando expressões regulares"
date:                  2024-01-19
simple_title:         "Utilizando expressões regulares"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## O Quê & Porquê?
Expressões regulares são padrões usados para encontrar ou substituir texto. Programadores as utilizam para validação de dados, busca e processamento de texto de forma eficiente.

## Como Fazer:
```c
#include <stdio.h>
#include <regex.h>

int main() {
    regex_t regex;
    int reti;
    char msgbuf[100];

    // Compila o regex para reconhecer a palavra "exemplo"
    reti = regcomp(&regex, "exemplo", 0);
    if (reti) { fprintf(stderr, "Não foi possível compilar o regex\n"); return 1; }

    // Executa o regex
    reti = regexec(&regex, "Isto é um exemplo simples.", 0, NULL, 0);
    if (!reti) {
        puts("Padrão encontrado!");
    } else if (reti == REG_NOMATCH) {
        puts("Padrão não encontrado!");
    } else {
        regerror(reti, &regex, msgbuf, sizeof(msgbuf));
        fprintf(stderr, "Erro ao processar regex: %s\n", msgbuf);
        return 1;
    }

    // Libera a estrutura regex utilizada
    regfree(&regex);

    return 0;
}
```
Saída:
```
Padrão encontrado!
```

## Detalhamento
Expressões regulares surgiram nos anos 50. Hoje, diversas linguagens as implementam, mas em C, normalmente usamos a biblioteca POSIX regex.h. Alternativas incluem bibliotecas de terceiros, como PCRE (Perl Compatible Regular Expressions). A implementação em C é mais verbosa comparada a linguagens de alto nível, mas oferece granularidade e controle.

## Veja Também
- Documentação POSIX regex.h: https://pubs.opengroup.org/onlinepubs/007904875/basedefs/regex.h.html
- PCRE: https://www.pcre.org/
- Tutorial de expressões regulares: https://www.regular-expressions.info/tutorial.html
