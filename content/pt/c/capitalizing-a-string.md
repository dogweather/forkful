---
title:    "C: Maiúsculas em uma string"
keywords: ["C"]
---

{{< edit_this_page >}}

## Por que

Capitalizar uma string é importante em muitos programas em C, especialmente quando se trata de entrada de dados do usuário. Ao capitalizar uma string, garantimos que todos os caracteres estejam em maiúsculas, o que facilita o processamento e a comparação de dados.

## Como Fazer

Para capitalizar uma string em C, podemos usar a biblioteca "string.h" e a função "toupper()", que converte um caractere em maiúsculo. Abaixo está um exemplo de código:

```C
#include <stdio.h>
#include <string.h>
#include <ctype.h>

int main()
{
    char string[20];

    printf("Digite uma string: ");
    scanf("%s", string);

    //converter cada caractere em maiúsculo
    for(int i = 0; i < strlen(string); i++)
    {
        string[i] = toupper(string[i]);
    }

    printf("String capitalizada: %s", string);

    return 0;
}

```

Este código solicitará ao usuário uma string e, em seguida, converterá cada caractere para maiúsculo usando um loop for e a função "toupper()". A saída será a string original, mas com todos os caracteres em maiúsculo.

Para fins de demonstração, se digitarmos "blog post" como entrada, a saída será "BLOG POST".

## Mergulho Profundo

É importante notar que a função "toupper()" só funcionará com caracteres ASCII padrão. Se nossos dados contiverem caracteres especiais, como acentos, a função não funcionará corretamente. Além disso, essa função só converte caracteres para maiúsculas, mas não lida com acentuação de letras maiúsculas e minúsculas em idiomas como o português.

Outra abordagem para capitalizar uma string é usar a função "strlwr()", que converte uma string inteira para minúsculas e, em seguida, use a função "toupper()" em cada primeiro caractere de cada palavra. Isso pode ser uma opção mais abrangente, mas requer mais código.

## Veja também

- Um guia completo sobre strings em C: [https://www.cprogramming.com/tutorial/c/lesson9.html](https://www.cprogramming.com/tutorial/c/lesson9.html)
- A documentação oficial da função "toupper()": [https://www.cplusplus.com/reference/cctype/toupper/](https://www.cplusplus.com/reference/cctype/toupper/)
- Outras funções de manipulação de strings em C: [http://www.cplusplus.com/reference/cstring/](http://www.cplusplus.com/reference/cstring/)