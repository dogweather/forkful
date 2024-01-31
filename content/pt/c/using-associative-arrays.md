---
title:                "Usando arrays associativos"
date:                  2024-01-30T19:10:31.713192-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando arrays associativos"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Que?

Arrays associativos, ou mapas de hash, são pares de chave-valor que permitem armazenar e recuperar dados com uma chave. Eles são incrivelmente úteis em C porque permitem um acesso mais rápido aos dados em comparação com listas, especialmente quando você está lidando com uma grande quantidade de dados.

## Como fazer:

C não possui suporte embutido para arrays associativos como algumas outras linguagens, mas podemos usar estruturas e algumas funções de bibliotecas para obter uma funcionalidade similar. Aqui está uma implementação simples usando a biblioteca `uthash`, que você precisará incluir em seu projeto.

Primeiro, defina uma estrutura para manter seus pares de chave-valor:

```C
#include <stdio.h>
#include "uthash.h"

typedef struct {
    int id; // Esta será nossa chave
    char name[10]; // Este é o valor associado à nossa chave
    UT_hash_handle hh; // Torna esta estrutura hashável
} person;
```

Em seguida, vamos adicionar algumas entradas e recuperá-las:

```C
int main() {
    person *my_people = NULL, *s;

    // Adicionando uma entrada
    s = (person*)malloc(sizeof(person));
    s->id = 1;
    strcpy(s->name, "Alice");
    HASH_ADD_INT(my_people, id, s);

    // Recuperando uma entrada
    int user_id = 1;
    HASH_FIND_INT(my_people, &user_id, s);
    if (s) {
        printf("Encontrado: %s\n", s->name);
    }
    
    return 0;
}
```

A saída de exemplo seria:

```
Encontrado: Alice
```

Não esqueça de liberar a memória alocada e desalocar a tabela de hash ao terminar para evitar vazamentos de memória.

## Aprofundamento

Embora arrays associativos não sejam nativos de C, bibliotecas como `uthash` preenchem bastante bem essa lacuna, oferecendo uma maneira relativamente simples de usar essa funcionalidade. Historicamente, os desenvolvedores de C tinham que implementar suas versões dessas estruturas de dados, levando a implementações variadas e muitas vezes complexas, especialmente para aqueles que estão apenas começando com a linguagem.

Lembre-se, a eficiência do uso de arrays associativos em C depende muito de quão bem a função de hash distribui valores pela tabela para minimizar colisões. Enquanto bibliotecas como `uthash` oferecem um bom equilíbrio entre facilidade de uso e desempenho, em aplicações críticas onde o desempenho é primordial, você pode querer personalizar ou implementar sua própria tabela de hash.

Para aplicações que exigem máxima eficiência, estruturas de dados alternativas ou até outras linguagens de programação com suporte embutido para arrays associativos podem ser uma escolha melhor. No entanto, para muitas situações, especialmente onde você já está trabalhando em um ambiente C, usar uma biblioteca como `uthash` proporciona um equilíbrio prático entre desempenho e conveniência.
