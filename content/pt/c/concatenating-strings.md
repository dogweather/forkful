---
title:                "C: Unindo strings"
simple_title:         "Unindo strings"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que concatenar strings em C?

A concatenação de strings é uma tarefa muito comum na programação em C. Ela permite combinar duas ou mais strings para formar uma nova string. Isso pode ser útil em situações em que você precisa juntar informações de diferentes fontes ou gerar uma saída formatada.

## Como fazer a concatenação de strings em C?

Para concatenar strings em C, você precisará usar a função `strcat()` da biblioteca padrão `string.h`. Esta função recebe duas strings como argumentos e retorna a concatenação das duas. Abaixo está um exemplo de código que mostra a concatenação de duas strings:

```
#include <stdio.h>
#include <string.h>

int main() {
   char saudacao[] = "Olá";
   char nome[] = "João";

   strcat(saudacao, nome);
   printf("A saudação é: %s", saudacao);
   
   return 0;
}
```
**Output:**

```
A saudação é: Olá João
```

Neste exemplo, a função `strcat()` combina a string "Olá" com a string "João" e armazena o resultado em `saudacao`. É importante notar que a primeira string precisa ter espaço suficiente para armazenar os caracteres da segunda string.

Você também pode concatenar mais de duas strings usando múltiplas chamadas da função `strcat()`. Por exemplo:

```
#include <stdio.h>
#include <string.h>

int main() {
   char saudacao[] = "Seja bem-vindo, ";
   char nome[] = "João";
   char ponto[] = "!";

   strcat(saudacao, nome);
   strcat(saudacao, ponto);
   printf("%s", saudacao);
   
   return 0;
}
```
**Output:**

```
Seja bem-vindo, João!
```

É importante lembrar que o limite de caracteres em uma string em C é determinado pelo tamanho do array em que ela está armazenada. Portanto, é essencial garantir que o array tenha espaço suficiente para armazenar as strings concatenadas.

## Profundidade na concatenação de strings

Um fato importante a ser lembrado ao usar a função `strcat()` é que ela irá adicionar a segunda string ao final da primeira string, inclusive o seu caractere nulo de terminação. Isso significa que a segunda string será "colada" no final da primeira string e não serão criados novos espaços em branco entre as strings.

Também é importante ressaltar que a função `strcat()` modifica a primeira string passada como argumento. Portanto, é uma boa prática criar uma nova string para armazenar a concatenação e deixar as strings originais intactas.

Além disso, é possível concatenar strings usando o operador `+` em vez da função `strcat()`. No entanto, esse método pode ser menos eficiente e pode causar problemas com memória se as strings forem muito grandes.

## Veja também

- [Documentação oficial da função `strcat()` em C](https://www.cplusplus.com/reference/cstring/strcat/)
- [Concatenando strings em C++](https://www.digitalocean.com/community/tutorials/how-to-concatenate-strings-in-cpp-2)
- [Vídeo tutorial sobre concatenar strings em C](https://www.youtube.com/watch?v=XjEgXEm4aUQ)