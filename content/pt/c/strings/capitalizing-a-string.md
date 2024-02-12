---
title:                "Capitalizando uma string"
aliases:
- /pt/c/capitalizing-a-string.md
date:                  2024-02-03T17:52:38.244720-07:00
model:                 gpt-4-0125-preview
simple_title:         "Capitalizando uma string"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Quê?

Capitalizar uma string em C envolve converter o primeiro caractere de cada palavra em uma dada string para maiúsculo, caso seja uma letra minúscula. Os programadores frequentemente realizam essa operação para padronizar a entrada do usuário para buscas, operações de ordenação ou propósitos de exibição, assegurando consistência e legibilidade através dos dados de texto.

## Como Fazer:

A capitalização de uma string em C requer um entendimento básico sobre manipulação de caracteres e percurso de string. Como C não possui uma função integrada para isso, tipicamente se verifica cada caractere, ajustando sua caixa conforme necessário. Abaixo está uma implementação simples:

```c
#include <stdio.h>
#include <ctype.h> // Para as funções islower e toupper

void capitalizeString(char *str) {
    if (str == NULL) return; // Verificação de segurança
    
    int capNext = 1; // Flag para indicar se o próximo caractere deve ser capitalizado
    for (int i = 0; str[i] != '\0'; i++) {
        if (capNext && islower(str[i])) {
            str[i] = toupper(str[i]); // Capitaliza o caractere
            capNext = 0; // Reseta a flag
        } else if (str[i] == ' ') {
            capNext = 1; // O próximo caractere deve ser capitalizado
        }
    }
}

int main() {
    char exemploString[] = "hello world. programming in c!";
    capitalizeString(exemploString);
    printf("String capitalizada: %s\n", exemploString);
    return 0;
}
```

Saída de Amostra:
```
String capitalizada: Hello World. Programming In C!
```

Este programa percorre a string `exemploString`, verificando cada caractere se deve ser capitalizado. A função `islower` verifica se um caractere é uma letra minúscula, enquanto `toupper` converte para maiúsculo. A flag `capNext` determina se a próxima letra encontrada deve ser convertida, sendo configurada após cada espaço (' ') encontrado, e inicialmente para capitalizar o primeiro caractere da string.

## Aprofundamento

A técnica demonstrada é direta mas carece de eficiência para strings muito grandes ou quando executada repetidamente em aplicações críticas de desempenho. Em contextos históricos e de implementação, a manipulação de strings em C, incluindo capitalização, frequentemente envolve manipulação direta do buffer, refletindo a abordagem de baixo nível de C e dando ao programador controle total sobre compromissos de memória e desempenho.

Existem métodos alternativos, mais sofisticados para capitalizar strings, especialmente ao considerar localidades e caracteres unicode, onde as regras de capitalização podem diferir significativamente do cenário ASCII simples. Bibliotecas como ICU (International Components for Unicode) fornecem soluções robustas para estes casos, mas introduzem dependências e sobrecarga que podem não ser necessárias para todas as aplicações.

Além disso, enquanto o exemplo fornecido usa as funções da Biblioteca Padrão C `islower` e `toupper`, que fazem parte de `<ctype.h>`, é essencial entender que estas funcionam dentro do intervalo ASCII. Para aplicações que demandam processamento de caracteres além do ASCII, como o manuseio de caracteres acentuados em idiomas europeus, lógica adicional ou bibliotecas de terceiros serão necessárias para realizar a capitalização de forma precisa.

Em conclusão, enquanto o método delineado é adequado para muitas aplicações, entender suas limitações e as alternativas disponíveis é crucial para desenvolver software robusto e internacionalizado em C.
