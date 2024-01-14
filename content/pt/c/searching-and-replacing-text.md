---
title:                "C: Busca e substituição de texto"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que utilizar busca e substituição de texto?

A busca e substituição de texto é uma ferramenta útil e importante na programação em C. Ela permite que você encontre e altere determinadas partes do seu código de forma rápida e eficiente. Além disso, com essa função, é possível automatizar tarefas repetitivas e economizar tempo.

## Como fazer busca e substituição de texto em C

A sintaxe básica para a busca e substituição de texto em C é a seguinte:

```
#include <stdio.h>
#include <string.h>

int main()
{
    char text[100];
    char search[20];
    char replace[20];
 
    printf("Digite o texto: ");
    scanf("%s", &text);
 
    printf("Digite a palavra que deseja buscar: ");
    scanf("%s", &search);
 
    printf("Digite a palavra que deseja substituir: ");
    scanf("%s", &replace);
    
    // Usando a função 'strstr' para encontrar a posição da palavra a ser substituída
    char *position = strstr(text, search); 
    
    // Usando a função 'strcpy' para copiar a parte do texto após a palavra a ser substituída
    strcpy(position, replace); 
    
    // Imprimindo o resultado final
    printf("O texto após a substituição é: %s", text);
 
    return 0;
}
```

Suponha que o usuário digite "Eu amo programação em C" como texto, "amo" como palavra de busca e "admiro" como palavra de substituição. O resultado final seria "Eu admiro programação em C".

## Mergulho Profundo na Busca e Substituição de Texto em C

Na busca e substituição de texto, a função `strstr` é usada para encontrar a posição da palavra a ser substituída. Ela retorna um ponteiro para a primeira ocorrência da palavra dentro do texto. É importante salientar que, caso a palavra de busca não seja encontrada, a função retorna NULL.

Em seguida, usamos a função `strcpy` para copiar a parte do texto que vem após a palavra a ser substituída. Essa parte do texto é adicionada ao ponteiro retornado pela função `strstr`. Então, a nova palavra é inserida nessa posição e a partir daí, o texto é impresso com a substituição feita.

Outra função importante na busca e substituição de texto é a `sprintf`, que permite que você formate uma string e a salve em uma variável. Em vez de imprimir o resultado na tela, você pode salvar a nova string em uma variável e usá-la posteriormente em seu código.

## Veja também

- [Tutorial de busca e substituição de texto em C](https://www.programiz.com/c-programming/examples/replace-character)
- [Documentação oficial da função `strstr()`](https://www.tutorialspoint.com/c_standard_library/c_function_strstr.htm)
- [Diferentes opções de substituição de texto em C](https://c-for-dummies.com/blog/?p=869)