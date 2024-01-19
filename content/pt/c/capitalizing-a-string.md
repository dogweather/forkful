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

## O Que é & Porquê?
Em programação, "capitalizar" uma string significa converter todas as letras em maiúsculas. Os programadores fazem isso para uniformizar dados, facilitando a comparação entre diferentes entradas.

## Como fazer:
Veja o exemplo de código abaixo que demonstra como capitalizar uma string em C.

```C 
#include <ctype.h>
#include <stdio.h>
#include <string.h>

void capitalize(char* s) {
    for(int i = 0; s[i]; i++) {
        s[i] = toupper((unsigned char) s[i]);
    }
}

int main() {
    char s[] = "Oi, como você está?";
    capitalize(s);
    printf("%s\n", s);  // "OI, COMO VOCÊ ESTÁ?"
    return 0;
}
```
Experimente compilar e executar este código. A saída será `OI, COMO VOCÊ ESTÁ?`.

## Mergulho Profundo
A função `toupper()` usada no código acima é uma contribuição histórica do padrão C89. Este padrão deu aos programadores o acesso às funções da biblioteca ctype.h que lidam com a conversão e classificação de caracteres. Contudo, a função `toupper()` só converte um caractere por vez. Quando temos que capitalizar uma string inteira, precisamos usar uma função personalizada, como a função `capitalize()` que criamos acima.

Existem outras maneiras de capitalizar uma string em C. Por exemplo, você pode usar a função `transform()` na biblioteca algoritmo C++ ou a biblioteca GNU `strupper()`. Mas essas não são soluções padronizadas e podem não funcionar em todas as plataformas.

## Veja também
- Documentação ctype.h: http://www.cplusplus.com/reference/cctype/
- Documentação toupper: https://www.cplusplus.com/reference/cctype/toupper/ 
- Documentação C99 Standard: http://www.open-std.org/jtc1/sc22/wg14/www/standards.html#9899