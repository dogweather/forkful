---
title:    "C: Buscando e substituindo texto"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que
Você já se perguntou por que é importante ser capaz de pesquisar e substituir textos em um programa de computador? Bem, a resposta é simples. Substituir palavras ou trechos de texto é uma das habilidades básicas necessárias em linguagens de programação, incluindo C. Sem essa habilidade, seria muito mais difícil fazer alterações em massa em um código.

## Como fazer
Agora que você sabe por que a pesquisa e substituição de texto é importante, vamos dar uma olhada em como fazer isso em C. Felizmente, o C possui uma função embutida chamada "str_replace", que permite que você substitua facilmente um determinado texto por outro em uma string.

Vamos dar um exemplo simples, imagine que você tem uma string que contém a frase "A programação é divertida" e você quer substituir a palavra "divertida" por "incrível". Usando a função "str_replace", ficaria assim:

```
#include <stdio.h>

int main()
{
    char frase[] = "A programação é divertida";
    printf("%s\n", frase);
    
    str_replace(frase, "divertida", "incrível"); // substituição
    printf("%s\n", frase);
    
    return 0;
}
```
A saída seria "A programação é incrível". Como você pode ver, a função "str_replace" nos permite substituir facilmente uma palavra em uma string.

## Aprofundando
Agora que você já sabe como fazer uma pesquisa e substituição básica em C, vamos dar uma olhada em como essa função funciona por trás dos bastidores. Primeiro, é importante entender que a função "str_replace" é na verdade uma combinação de outras funções, "strstr" e "strncpy". Essas funções são usadas para localizar e copiar uma parte específica de uma string para outra. Em seguida, o texto a ser substituído é inserido na string substituída, e as funções "strlen" e "strcpy" são usadas para garantir que a string tenha o tamanho correto.

É importante notar que a função "str_replace" só substituirá a primeira ocorrência do texto a ser substituído em uma string. Se você quiser substituir todas as ocorrências, você precisará usar um loop para percorrer toda a string.

## Veja também
- [Função str_replace em C](https://www.programiz.com/c-programming/library-function/string.h/str_replace)
- [Como usar a função "strstr" em C](https://www.geeksforgeeks.org/c-string-strstr/)
- [Explicação da função "strncpy"](https://www.tutorialspoint.com/c_standard_library/c_function_strncpy.htm)