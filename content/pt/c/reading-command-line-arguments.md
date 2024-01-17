---
title:                "Lendo argumentos da linha de comando"
html_title:           "C: Lendo argumentos da linha de comando"
simple_title:         "Lendo argumentos da linha de comando"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

O que e por que?

Ler os argumentos da linha de comando significa que o programa pode receber informações do usuário através da linha de comando ao ser executado. Os programadores fazem isso para permitir que seus programas sejam mais dinâmicos e personalizáveis, permitindo que os usuários forneçam os dados necessários para a execução do programa.

Como fazer:

````C
#include <stdio.h>
int main(int argc, char *argv[]){
   int i;
   
   printf("Foram fornecidos %d argumentos na linha de comando\n", argc-1);
   
   for (i = 1; i < argc; i++){
       printf("Argumento %d: %s\n", i, argv[i]);
   }
   
   return 0;
}
````

Exemplo de saída para o comando "./programa teste1 teste2 teste3":

````bash
Foram fornecidos 3 argumentos na linha de comando
Argumento 1: teste1
Argumento 2: teste2
Argumento 3: teste3
````

Mergulho profundo:

Ler os argumentos da linha de comando é uma técnica muito antiga, que remonta aos primeiros sistemas operacionais. Originalmente, era usado para permitir que o usuário fornecesse parâmetros para a execução do programa, devido às limitações dos sistemas dos anos 50 e 60. Atualmente, existem alternativas mais avançadas, como a leitura de variáveis de ambiente. A implementação dos argumentos da linha de comando em um programa pode ser feita de várias maneiras, sendo as mais comuns o uso da função "main" com parâmetros "int argc" e "char *argv[]".

Veja também:

- [Documentação oficial do C](https://www.ibm.com/docs/pt/c-help-official-product-documentation-bundle/guide/topics/compatibility-pointers.html)
- [Tutorial de linha de comando do C](https://www.learn-c.org/en/Command_Line_Arguments)