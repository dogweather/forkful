---
title:    "C: Escrevendo para a saída de erro padrão"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que escrever em standard error em programação C?

Escrever em standard error pode ser uma ferramenta útil em programação C quando se quer detectar e identificar possíveis erros durante a execução do código. Ao imprimir mensagens de erro em standard error, o programador pode facilmente identificar onde o problema ocorre e iniciar a resolução do mesmo com mais facilidade.

## Como fazer?

Usar a função "fprintf" é a forma mais comum de escrever em standard error. Vamos ver um exemplo de como essa função pode ser utilizada em um código:

```C
#include <stdio.h>

int main() {
    char* texto = "Hello World!";
    fprintf(stderr, "Imprimindo uma mensagem de erro: %s\n", texto);
    return 0;
}
```

Ao executar esse código, a mensagem de erro será impressa em standard error, com a formatação especificada na função "fprintf". O resultado será:

`Imprimindo uma mensagem de erro: Hello World!`

Outra forma de escrever em standard error é utilizando o operador `>>` em conjunto com "2". Por exemplo:

`./meu_programa 2>> log.txt`

Isso irá redirecionar qualquer mensagem de erro que ocorra durante a execução do programa para um arquivo de texto chamado "log.txt". Isso pode ser útil para armazenar e analisar erros em um momento posterior.

## Profundidade

Além da função "fprintf" e do redirecionamento para arquivo, existem outras formas de escrever em standard error em programação C, como por exemplo a função "perror" e o comando "stderr". Cada uma dessas opções pode ser mais adequada dependendo do contexto e do objetivo de uso.

Outro ponto importante a mencionar é que é possível personalizar as mensagens de erro a serem impressas em standard error. Dessa forma, é possível criar mensagens mais específicas e úteis ao invés de apenas imprimir o nome e a linha do erro.

## Veja também
- [Documentação sobre a função fprintf](https://www.cplusplus.com/reference/cstdio/fprintf/)
- [Guia sobre redirecionamento de saída em C](https://www.tutorialspoint.com/how-to-redirect-output-to-a-file-in-c)
- [Artigo sobre a função perror](https://www.geeksforgeeks.org/perror-c-library-function/)
- [Diferenças entre standard output e standard error](https://stackoverflow.com/questions/2342826/what-are-the-differences-between-stderr-and-stdout)