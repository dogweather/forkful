---
title:                "Organizando o código em funções"
date:                  2024-01-26T01:09:38.098310-07:00
model:                 gpt-4-1106-preview
simple_title:         "Organizando o código em funções"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## O Que & Por Que?
Organizar o código em funções consiste em dividir o código em blocos reutilizáveis que realizam tarefas específicas. Isso torna o código mais fácil de ler, depurar e manter.

## Como fazer:
Vamos pegar um exemplo simples: digamos que você queira adicionar dois números várias vezes.

Sem funções:
```C
#include <stdio.h>

int main() {
    int soma1 = 5 + 3;
    printf("Soma1: %d\n", soma1);
    
    int soma2 = 2 + 8;
    printf("Soma2: %d\n", soma2);
    
    // Mais adições aqui...
    
    return 0;
}
```

Com funções:
```C
#include <stdio.h>

int adicionar(int a, int b) {
    return a + b;
}

int main() {
    int soma1 = adicionar(5, 3);
    printf("Soma1: %d\n", soma1);
    
    int soma2 = adicionar(2, 8);
    printf("Soma2: %d\n", soma2);
    
    // Use a função adicionar() para mais adições...
    
    return 0;
}
```

Saída:
```
Soma1: 8
Soma2: 10
```

## Aprofundamento
Antes de o C ter funções, a programação era frequentemente feita de maneira linear, quase como uma receita. Mas à medida que os programas cresceram, a duplicação de código se tornou um problema. As funções foram a solução - elas nos permitiram executar o mesmo bloco de código de diferentes partes de um programa sem reescrevê-lo todas as vezes. Isso não apenas economiza espaço, mas também tempo ao fazer atualizações: altere a função em um lugar, e todas as partes do seu código que a usam recebem a atualização.

Alternativas às funções podem incluir código inline, macros ou programação de cópia e cola, mas essas podem levar a códigos inchados, propensos a erros e difíceis de manter. As funções, em contraste, encapsulam a funcionalidade, definem interfaces claras e podem reduzir os efeitos colaterais com o uso adequado do escopo.

Quando você está implementando funções, considere alguns detalhes: primeiro, tente fazer com que elas façam apenas uma coisa – isso é conhecido como o Princípio da Responsabilidade Única. Segundo, os nomes são importantes – escolha nomes descritivos para funções e seus parâmetros para tornar seu código autoexplicativo.

## Veja Também
Para saber mais sobre funções em C, dê uma olhada nestes:

- Referência da Biblioteca Padrão C: https://en.cppreference.com/w/c/header
- Programação C: Uma Abordagem Moderna por K.N. King: Um livro com um aprofundamento sobre funções.
- Learn-C.org: Seção de funções: https://www.learn-c.org/en/Functions
