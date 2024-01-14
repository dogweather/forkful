---
title:    "C: Escrevendo para o erro padrão"
keywords: ["C"]
---

{{< edit_this_page >}}

## Por que escrever para o erro padrão?

Escrever para o erro padrão é uma técnica utilizada em programação para comunicar informações de erro ou debug para o usuário do programa. Isso é especialmente útil quando se trata de programas que não possuem uma interface gráfica e precisam reportar erros de forma clara para o usuário.

## Como Fazer

Para escrever para o erro padrão em C, utilizamos a função "fprintf". Veja um exemplo abaixo:

```C
#include <stdio.h>

int main(){
    fprintf(stderr, "Um erro ocorreu!");
    return 0;
}
```

Neste código, utilizamos a diretiva "#include" para incluir a biblioteca "stdio.h" que contém a função "fprintf". Dentro da função, escrevemos a mensagem de erro entre as aspas e utilizamos "stderr" como argumento, indicando que queremos escrever para o erro padrão. O código acima resultaria em uma saída semelhante a esta:

```bash
Um erro ocorreu!
```

Podemos adicionar mais informações ao código, incluindo variáveis:

```C
#include <stdio.h>

int main(){
    int num = 10;
    fprintf(stderr, "Erro: A variável 'num' possui o valor de %d.", num);
    return 0;
}
```

Neste caso, a saída seria: 

```bash
Erro: A variável 'num' possui o valor de 10.
```

## Deep Dive

Ao escrever para o erro padrão, é importante lembrar que esta é uma técnica de comunicação com o usuário, por isso é necessário escrever mensagens claras e informativas. Além disso, é importante também fornecer informações suficientes para que o usuário possa entender e resolver o erro. 

A função "fprintf" possui três argumentos: o primeiro é o stream para o qual queremos escrever (no caso, "stderr"), o segundo é a string com a mensagem a ser escrita e o terceiro são os argumentos adicionais, que podem ser variáveis ou strings, por exemplo. 

Outro ponto importante é que ao escrever para o erro padrão, a mensagem será exibida imediatamente, sem a necessidade de um comando como "printf" que geralmente utiliza o buffer para exibir a saída. Isso garante que a mensagem de erro será exibida corretamente e no momento em que ocorrer.

## Veja Também

- [Documentação oficial da função fprintf em C](https://www.gnu.org/software/libc/manual/html_node/Formatted-Output-Functions.html#Formatted-Output-Functions)
- [Tutorial sobre escrita para o erro padrão em C](https://www.geeksforgeeks.org/error-handling-c-programs/)
- [Artigo sobre como escrever mensagens de erro eficientes](https://www.cprogramming.com/tutorial/error-handling.html)