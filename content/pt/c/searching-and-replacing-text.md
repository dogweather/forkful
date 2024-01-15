---
title:                "Buscando e substituindo texto"
html_title:           "C: Buscando e substituindo texto"
simple_title:         "Buscando e substituindo texto"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que

Você já se deparou com um grande texto ou código e precisou realizar uma mudança em várias partes dele? Isso pode ser uma tarefa cansativa e demorada se for feita manualmente. Felizmente, no C, existe uma maneira fácil de encontrar e substituir parte do texto, economizando tempo e esforço.

## Como Fazer

Para encontrar e substituir texto em um programa C, você precisará utilizar a função `strstr()`. Essa função busca a ocorrência de uma string dentro de outra, retornando um ponteiro para a primeira ocorrência. Em seguida, você pode usar a função `strcpy()` para substituir a string encontrada pela nova string desejada.

Por exemplo, se quisermos substituir a palavra "mundo" pela palavra "futuro" em uma string, podemos fazer o seguinte:

```
char texto[100] = "Olá, mundo!";
char *posicao = strstr(texto, "mundo");
strcpy(posicao, "futuro");
printf("%s", texto);
```

Isso resultará em um texto com a seguinte saída: "Olá, futuro!".

## Mergulho Profundo

Além da função `strstr()`, existem outras opções para realizar a busca e substituição de texto em C. Uma delas é a função `strtok()`, que divide uma string em tokens, permitindo uma busca mais específica em cada parte do texto.

Outra opção é o uso de expressões regulares, que permitem uma busca ainda mais complexa e precisa. Para isso, é necessário incluir a biblioteca `regex.h` e utilizar as funções `regcomp()` e `regexec()` para compilar e executar a expressão regular desejada.

Lembre-se de sempre tomar cuidado com as alterações feitas no seu código durante o processo de busca e substituição de texto. Certifique-se de ter feito um backup do seu código antes de realizar qualquer mudança.

## Veja Também

- Link 1: https://www.tutorialspoint.com/c_standard_library/c_function_strstr.htm
- Link 2: https://www.geeksforgeeks.org/c-program-find-replace-word-text-file/
- Link 3: https://www.gnu.org/software/libc/manual/html_node/String-Matching.html