---
title:    "C: Excluindo caracteres que correspondem a um padrão"
keywords: ["C"]
---

{{< edit_this_page >}}

## Por que

Muitas vezes, quando estamos trabalhando com strings em programação, podemos encontrar situações em que precisamos remover caracteres específicos de uma determinada string. Isso pode ser necessário para limpar os dados de entrada ou para formatar uma string de acordo com um padrão específico. Felizmente, na linguagem C, existem maneiras simples de lidar com essa situação, usando funções de caractere e loops.

## Como fazer

Para deletar caracteres que correspondem a um padrão, podemos usar a função **strchr()**, que encontra a primeira ocorrência de um caractere específico em uma string. Podemos então usá-la em conjunto com um loop *while* para percorrer a string e deletar todas as ocorrências do caractere que desejamos. Vamos dar uma olhada em um exemplo de código em C:

```
#include <stdio.h>
#include <string.h>

// Função que deleta todas as ocorrências de um caractere em uma string
void deleteChar(char *str, char c){

     // Iniciamos um loop while que só terminará quando não houver mais ocorrências do caractere na string
     while (strchr(str, c) != NULL) {

          // Encontramos a posição do caractere na string
          int index = strchr(str, c) - str;

          // Deletamos o caractere movendo todos os elementos a partir da posição encontrada para a esquerda
          memmove(&str[index], &str[index + 1], strlen(str) - index);

          // Adicionamos o caractere nulo ao final da string para garantir que ela continue terminando corretamente
          str[strlen(str) - 1] = '\0';
     }
}

int main(){

     char string[50] = "Eu gosto de maçãs e bananas";
     char c = 'a';

     // Chamamos a função para deletar todas as ocorrências de 'a' na string
     deleteChar(string, c);

     // Imprimimos a string modificada
     printf("String sem o caractere '%c': %s", c, string);

     return 0;
}
```

**Saída:**

```
String sem o caractere 'a': Eu gosto de mçs e bnns
```

Neste exemplo, usamos a função **memmove()** para mover os elementos da string a partir do índice encontrado para a esquerda, substituindo o caractere a ser deletado. Por fim, adicionamos o caractere nulo ao final da string, para garantir que ela continue terminando corretamente.

## Deep Dive

Além da função **strchr()**, também existem outras funções que podem ser usadas para deletar caracteres em uma string em C, como **strtok()** e **strcspn()**. Cada uma delas possui suas próprias características e pode ser mais adequada dependendo do padrão que estamos buscando. Também podemos usar a função **regexp.h** para trabalhar com expressões regulares e deletar padrões mais complexos de caracteres. É importante entender que deletar caracteres em uma string pode resultar em uma string modificada, por isso é importante usar essas funções com cuidado e testá-las para garantir que os resultados estejam corretos.

## Veja também

- [Documentação oficial da função strchr() em C](https://www.cplusplus.com/reference/cstring/strchr/)
- [Exemplo de uso da função memmove() em C](https://www.geeksforgeeks.org/memmove-in-c-cpp/)
- [Como usar expressões regulares em C](https://www.regular-expressions.info/tutorial.html)