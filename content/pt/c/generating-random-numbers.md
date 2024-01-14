---
title:                "C: Geração de números aleatórios"
programming_language: "C"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que gerar números aleatórios é importante

Gerar números aleatórios é uma tarefa muito comum em muitos programas de computador. Isso permite que o programa crie resultados diferentes a cada vez que é executado, tornando-o mais dinâmico e interessante. Além disso, a geração de números aleatórios é útil em jogos, simulações e criptografia.

## Como gerar números aleatórios em C

Gerar números aleatórios em um programa é muito fácil com a linguagem de programação C. A biblioteca padrão "stdlib.h" contém a função "rand()", que retorna um número aleatório entre 0 e "RAND_MAX" (uma constante definida na biblioteca). Abaixo está um exemplo simples de como gerar três números aleatórios:

```C
#include <stdio.h>
#include <stdlib.h>

int main() {
    int numero1, numero2, numero3;

    // Gera três números aleatórios
    numero1 = rand();
    numero2 = rand();
    numero3 = rand();

    // Imprime os números gerados
    printf("Numero 1: %d \nNumero 2: %d \nNumero 3: %d", numero1, numero2, numero3);

    return 0;
}
```

A saída deste programa pode ser algo como:

```
Numero 1: 1804289383 
Numero 2: 846930886 
Numero 3: 1681692777
```

## Aprofundando na geração de números aleatórios

A função "rand()" funciona gerando uma sequência de números pseudoaleatórios baseados em um número chamado "seed" (semente). A cada vez que o programa é executado, a função usa a semente para gerar a mesma sequência de números. Para evitar isso e obter números realmente aleatórios, podemos usar a função "srand()" para definir uma semente diferente a cada vez que o programa é executado. Abaixo está um exemplo de como gerar três números aleatórios diferentes a cada vez que o programa é executado:

```C
#include <stdio.h>
#include <stdlib.h>
#include <time.h> // Incluímos a biblioteca "time.h" para usar a função time()

int main() {
    int numero1, numero2, numero3;

    // Define a semente baseada no tempo atual
    srand(time(NULL));

    // Gera três números aleatórios
    numero1 = rand();
    numero2 = rand();
    numero3 = rand();

    // Imprime os números gerados
    printf("Numero 1: %d \nNumero 2: %d \nNumero 3: %d", numero1, numero2, numero3);

    return 0;
}
```

Agora, a saída do programa será diferente a cada vez que for executado:

```
Numero 1: 499451735 
Numero 2: 251137083 
Numero 3: 105979497
```

## Veja também

Aqui estão alguns links úteis para se aprofundar ainda mais no assunto:

- [Documentação da função "rand()" na biblioteca padrão de C](https://www.cplusplus.com/reference/cstdlib/rand/)
- [Tutorial sobre geração de números aleatórios em C](https://www.gnu.org/software/gsl/doc/html/rng.html)
- [Artigo sobre a importância da geração de números aleatórios criptograficamente seguros](https://www.gnupg.org/documentation/manuals/gcrypt/Random-Numbers.html)