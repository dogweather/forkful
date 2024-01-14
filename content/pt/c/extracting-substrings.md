---
title:    "C: Extraindo Substrings"
keywords: ["C"]
---

{{< edit_this_page >}}

## Por que extrair substrings em linguagem C?

Extrair substrings, ou trechos de uma string, pode ser útil em diversas situações em programação. Pode ser necessário manipular uma string para obter apenas uma parte dela, ou até mesmo para separar informações específicas de um texto maior. Além disso, a extração de substrings pode ser uma técnica importante em algoritmos de processamento de texto.

## Como extrair substrings em linguagem C?

Extrair substrings em C é uma tarefa simples, mas requer conhecimento básico de strings e ponteiros. Um exemplo de código que extrai uma substring de uma string seria o seguinte:

```C
char str[] = "Esta é uma string de teste.";
char sub[10];
int inicio = 5;
int tamanho = 4;
strncpy(sub, str + inicio, tamanho);
printf("A substring é %s\n", sub);
```

Neste exemplo, a função `strncpy` é usada para copiar 4 caracteres da string original, a partir do índice 5, para a substring `sub`. O resultado da impressão seria "substring é uma ".

## Mergulho Profundo: Mais informações sobre a extração de substrings

Para entender melhor a extração de substrings em linguagem C, é importante compreender como as strings são armazenadas na memória. Em C, uma string é um array de caracteres finalizado com o caracter nulo `\0`. Assim, ao utilizar ponteiros e índices, é possível acessar trechos específicos de uma string e copiá-los para outro local na memória.

Além da função `strncpy`, também existem outras funções para manipular strings, como `strncat` e `strncat`, que podem ser úteis na extração de substrings. É importante observar que algumas dessas funções podem não incluir o `\0` no final da substring, por isso é necessário adicioná-lo manualmente caso a substring seja utilizada como uma string independente.

## Veja também

- [Documentação oficial da função strncpy em C](https://www.cplusplus.com/reference/cstring/strncpy/)
- [Como manipular strings em linguagem C](https://www.programiz.com/c-programming/c-strings)
- [Tutorial sobre ponteiros e arrays em C](https://www.ime.usp.br/~pf/algoritmos/aulas/pont.html)