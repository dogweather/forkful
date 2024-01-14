---
title:                "C: Capitalizando uma string"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Porquê
Em programação, é comum que as strings (cadeias de caracteres) sejam usadas para armazenar e manipular dados. Uma das tarefas comuns na manipulação de strings é capitalizá-las, ou seja, transformar todas as letras minúsculas em maiúsculas. Esta tarefa pode ser útil em diversas situações, como, por exemplo, na formatação de nomes ou textos para exibição.

## Como Fazer
Em C, existem diversas formas de capitalizar uma string. Uma maneira simples, porém menos eficiente, é percorrer a string caractere por caractere e utilizar a função `toupper()` para transformar cada letra em maiúscula. Veja um exemplo abaixo:

```C
#include <stdio.h>
#include <ctype.h> //biblioteca para utilizar a função toupper()

int main(void) {
    char str[] = "esta é uma frase em minúsculas.

    for (int i = 0; str[i] != '\0'; i++) {
        str[i] = toupper(str[i]);
    }

    printf("%s", str);

    return 0;
}
```
Saída:
```
ESTA É UMA FRASE EM MINÚSCULAS.
```
Um método mais eficiente é usar a função `strlwr()` para transformar a string inteira para minúsculas e depois utilizar a função `toupper()` apenas no primeiro caractere. Dessa forma, economizamos várias iterações no loop. Veja o exemplo abaixo:

```C
#include <stdio.h>
#include <ctype.h> //biblioteca para utilizar a função toupper()
#include <string.h> //biblioteca para utilizar a função strlwr()

int main(void) {
    char str[] = "esta é uma frase em minúsculas.";

    strlwr(str); //transforma toda a string em minúsculas
    str[0] = toupper(str[0]); //transforma apenas o primeiro caractere em maiúscula

    printf("%s", str);

    return 0;
}
```
Saída:
```
Esta é uma frase em minúsculas.
```

## Profundidade
Para compreender melhor como a função `toupper()` funciona, é importante entender que ela está presente na biblioteca `ctype.h` e que recebe como parâmetro um caractere. Se esse caractere for uma letra minúscula, a função retorna a letra maiúscula correspondente. Caso contrário, ela retorna o próprio caractere. Já a função `strlwr()` está presente na biblioteca `string.h` e percorre toda a string, convertendo cada caractere para minúscula. Essas funções podem ser bastante úteis em diferentes situações de manipulação de strings.

## Veja Também
- [Documentação do C - String Handling Functions](https://www.tutorialspoint.com/c_standard_library/c_function_strtol.htm)
- [Como usar String Handling Functions em C](https://www.geeksforgeeks.org/string-handling-functions-in-c/)
- [Entendendo a manipulação de strings em C](https://www.freecodecamp.org/news/string-manipulation-in-c-programming-language-6ef0ed2d79a2/)