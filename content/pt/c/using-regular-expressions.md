---
title:                "C: Utilizando expressões regulares"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por que Utilizar Expressões Regulares em Programação?
As expressões regulares são uma ferramenta poderosa em programação, permitindo a busca e manipulação de padrões de texto de forma eficiente. Com elas, é possível criar algoritmos complexos para validar ou extrair informações de strings de texto.

## Como Utilizar Expressões Regulares em Linguagem C
Para utilizar expressões regulares em C, é necessário incluir a biblioteca "regex.h" e utilizar as funções específicas dessa biblioteca. Veja um exemplo simples de como encontrar uma palavra em um texto com regex:

```C
#include <stdio.h>
#include <regex.h>

int main() {
    // Expressão regular para encontrar a palavra "programação"
    regex_t regex;
    int ret = regcomp(&regex, "programação", 0);
    if (ret) {
        // Tratamento de erro
        printf("Erro ao compilar expressão regular\n");
        return 1;
    }

    // Texto onde será feita a busca
    char *texto = "Esta é uma frase de exemplo sobre programação";

    // Variável para armazenar resultados da busca
    regmatch_t matches;

    // Função para procurar a expressão regular no texto
    ret = regexec(&regex, texto, 1, &matches, 0);
    if (ret == 0) {
        // Se a função retornar 0, significa que a palavra foi encontrada
        printf("Palavra encontrada na posição %d\n", matches.rm_so);
    } else {
        // Caso contrário, a palavra não foi encontrada
        printf("Palavra não encontrada\n");
    }

    // Libera memória alocada pela expressão regular
    regfree(&regex);

    return 0;
}
```

## Aprofundando-se em Expressões Regulares
Além de simplesmente encontrar e validar padrões, as expressões regulares permitem realizar substituições em textos, tornando o processo de manipulação de strings ainda mais versátil. É importante estudar bem a sintaxe das expressões regulares e praticar bastante para dominar seu uso.

Algumas dicas de recursos para aprender mais sobre expressões regulares em C:

- [Documentação oficial da biblioteca regex.h](https://pubs.opengroup.org/onlinepubs/009696899/basedefs/regex.h.html)
- [Tutorial de Expressões Regulares em C](https://linux.die.net/man/3/regex)
- [Cheat Sheet de Expressões Regulares em C](https://www.rexegg.com/regex-c-cpp.html)

## Veja Também
- [Tutorial de Expressões Regulares em Python](https://devblogs.microsoft.com/python/python-regular-expressions-the-ultimate-tutorial/)
- [Guia Rápido de Expressões Regulares em Java](https://www.vogella.com/tutorials/JavaRegularExpressions/article.html)
- [Ferramenta Online para Testar Expressões Regulares](https://regex101.com/)