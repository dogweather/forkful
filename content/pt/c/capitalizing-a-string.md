---
title:                "Capitalizando uma string"
html_title:           "C: Capitalizando uma string"
simple_title:         "Capitalizando uma string"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# O que é e por que fazemos isso?

Ao programar em C, pode ser necessário alterar a capitalização de uma string. Isso significa mudar todas as letras para maiúsculas ou minúsculas. Os programadores geralmente fazem isso para padronizar o formato de entrada ou saída de dados ou para facilitar a comparação entre strings.

# Como fazer:

Aqui está um exemplo simples de como capitalizar uma string em C:

```C
#include <stdio.h>
#include <string.h>

// Função para capitalizar uma string
void capitalize(char* str) {
    int i;
    // Loop através de todos os caracteres da string
    for(i = 0; i < strlen(str); i++) {
        // Se o caractere atual for uma letra minúscula
        if(str[i] >= 'a' && str[i] <= 'z') {
            // Subtrai 32 para mudá-lo para maiúscula
            str[i] = str[i] - 32;
        }
    }
}

// Função principal
int main() {
    char string[] = "exemplo de string com várias letras maiúsculas e minúsculas";
    // Chama a função capitalize
    capitalize(string);
    // Imprime a string capitalizada
    printf("%s", string);
    return 0;
}

```

Saída:

```
EXEMPLO DE STRING COM VÁRIAS LETRAS MAIÚSCULAS E MINÚSCULAS
```

# Mais informações:

A capitalização de strings pode ser rastreada até os primeiros dias da linguagem de programação C. Antes disso, era comum que os programadores usassem outras linguagens de programação, como Assembly, que não distinguiam maiúsculas de minúsculas. No entanto, com o aumento da complexidade dos programas, tornou-se importante padronizar a capitalização para facilitar a comparação entre strings.

Existem outras maneiras de capitalizar uma string em C, como o uso da função ```toupper()``` na biblioteca ```<ctype.h>```. No entanto, a abordagem mostrada neste exemplo é mais simples e direta, especialmente em casos em que apenas letras minúsculas precisam ser capitalizadas.

Uma consideração importante ao capitalizar strings é que o código acima só funcionará para caracteres ASCII. Para suportar outros tipos de conjunto de caracteres, é necessário uma abordagem mais complexa, utilizando funções como ```tolower()```, que podem lidar com caracteres acentuados.

# Veja também:

Mais informações sobre a função ```toupper()```:
https://www.programiz.com/c-programming/library-function/ctype.h/toupper