---
title:                "Capitalizando uma string"
date:                  2024-01-19
html_title:           "Bash: Capitalizando uma string"
simple_title:         "Capitalizando uma string"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## O que é & Porquê?
Capitalizar uma string significa converter todas as letras minúsculas em maiúsculas. Programadores fazem isso para padronizar dados, enfatizar palavras-chave ou atender requisitos estéticos e técnicos.


## Como Fazer:
```C
#include <stdio.h>
#include <ctype.h>  // Necessário para a função toupper

void capitalizeString(char *str) {
    while(*str) {
        *str = toupper((unsigned char) *str);
        str++;
    }
}

int main() {
    char text[] = "C é bacana!";
    printf("Original: %s\n", text);
    capitalizeString(text);
    printf("Capitalizada: %s\n", text);
    return 0;
}
```
Saída:
```
Original: C é bacana!
Capitalizada: C É BACANA!
```

## Aprofundando
Originalmente, a necessidade de capitalizar strings vem dos primórdios da computação, quando os terminais e impressoras trabalhavam somente com letras maiúsculas. Hoje em dia, além de questões de estilo, capitalizar strings pode ser útil para garantir a consistência durante comparações de texto, por exemplo, nomes de usuário em um sistema.

Alternativas ao `toupper` incluem a criação de funções personalizadas que percorrem uma string manualmente, convertendo cada caractere usando aritmética de ASCII, mas isso não é recomendado devido a problemas com codificação e localização.

Na implementação, é crucial considerar a codificação de caracteres. O `toupper` é parte do padrão C e lida bem com o conjunto de caracteres ASCII. Cuidados especiais devem ser tomados ao lidar com codificações além do ASCII, como UTF-8 ou quando caracteres acentuados estão envolvidos, para os quais bibliotecas e funções específicas são necessárias.

## Veja Também:
- [Documentação do toupper](https://www.cplusplus.com/reference/cctype/toupper/)
- [String Handling in C (cpluscplus.com)](https://www.cplusplus.com/reference/cstring/)
