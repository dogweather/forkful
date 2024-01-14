---
title:    "C: Convertendo uma string para letras minúsculas"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que converter uma string para letras minúsculas em C?

Muitas vezes, ao lidar com strings em um programa em C, pode ser necessário converter todas as letras em minúsculas. Isso é especialmente útil quando se trata de comparações de strings, pois torna a comparação mais precisa e evita erros devido a letras maiúsculas ou minúsculas diferentes. Além disso, converter uma string para minúsculas pode facilitar a manipulação de dados em seu programa.

## Como fazer isso em C

Felizmente, a linguagem C possui uma função embutida para converter uma string em minúsculas. A função é chamada de `tolower()` e está incluída na biblioteca padrão `<ctype.h>`. Para usá-la, primeiro é necessário declarar a string que deve ser convertida e, em seguida, chamar a função `tolower()` com essa string como parâmetro. Veja um exemplo de código abaixo:

```C
#include <stdio.h>
#include <ctype.h>

int main() {
    char frase[50] = "Olá Mundo!";
    int i = 0;

    while (frase[i]) {
        frase[i] = tolower(frase[i]);
        i++;
    }

    printf("%s\n", frase);

    return 0;
}
```

Neste exemplo, a string "Olá Mundo!" é convertida para "olá mundo!". Note que um loop `while` é utilizado para percorrer todos os caracteres da string e a função `tolower()` é aplicada a cada um deles.

O resultado da execução deste código é:

```
olá mundo!
```

## Dando um mergulho mais profundo

Além da função `tolower()`, existem outras formas de converter uma string em C para minúsculas. Uma maneira é usando o operador ternário `?:` e aplicando aritmética bit a bit para alterar o valor ASCII de cada caractere. Isso é útil em situações onde não se quer utilizar a biblioteca `<ctype.h>`.

Outra opção é utilizar a função `strlwr()` da biblioteca `<string.h>`, que é especificamente projetada para converter strings inteiras em minúsculas em uma única chamada. No entanto, essa função só está disponível em algumas versões da linguagem C.

## Veja também
- Tutorial sobre a função `tolower()` em C: https://www.tutorialspoint.com/c_standard_library/c_function_tolower.htm
- Explicações detalhadas sobre operações bit a bit: https://www.geeksforgeeks.org/bitwise-operators-in-c-cpp/
- Descrição da função `strlwr()` em C: https://www.geeksforgeeks.org/strlwr-function-in-c/