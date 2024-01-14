---
title:    "C: Convertendo uma data em uma string"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que

Converter uma data em uma string é uma tarefa comum em muitos programas de computador. Isso permite que os usuários visualizem as datas em um formato mais compreensível e de acordo com suas preferências.

## Como fazer

Existem diferentes abordagens para converter uma data em uma string na linguagem de programação C. Aqui estão dois exemplos:

```C
#include <stdio.h>
#include <time.h>

int main() {
    time_t t = time(NULL); // obter data e hora atual
    char str[100]; // armazenar string aqui

    // usando strftime()
    strftime(str, 100, "%d/%m/%Y", localtime(&t));
    printf("Data atual em formato string: %s\n", str);

    // usando sprintf()
    sprintf(str, "%d de %B de %Y", localtime(&t)->tm_mday, localtime(&t)->tm_mon+1, localtime(&t)->tm_year+1900);
    printf("Data atual em formato string: %s\n", str);

    return 0;
}
```

Aqui, usamos as funções `strftime()` e `sprintf()` para formatar a data de maneiras diferentes. A função `strftime()` aceita três argumentos: uma string para armazenar a data, o tamanho máximo da string e o formato da data. O formato é definido usando símbolos especiais, como "%d" para o dia, "%m" para o mês e "%Y" para o ano. A função `sprintf()` segue uma abordagem semelhante, mas usa `printf()` para formatar a data e, em seguida, armazena o resultado na string.

## Mergulho profundo

Para nos aprofundarmos um pouco mais no processo de converter datas em strings, podemos explicar brevemente como as datas são armazenadas em um programa. Dates são armazenados em um formato numérico, conhecido como "timestamp". Isto é, o número de segundos decorridos desde 1º de janeiro de 1970, 00:00:00 UTC. Quando precisamos exibir essa data para os usuários, usamos funções de formatação, como `strftime()`, para converter o timestamp em uma string legível.

## Veja também

- Documentação oficial da função strftime() do C: https://www.gnu.org/software/libc/manual/html_node/Formatting-Calendar-Time.html
- Documentação oficial da função sprintf() do C: https://www.gnu.org/software/libc/manual/html_node/Formatted-Output-Functions.html#Formatted-Output-Functions
- Tutorial sobre conversão de datas para strings em C: https://www.tutorialspoint.com/c_standard_library/c_function_strftime.htm