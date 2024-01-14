---
title:    "C: Obtendo a data atual"
keywords: ["C"]
---

{{< edit_this_page >}}

## Por que obter a data atual é importante?

Obter a data atual é uma tarefa comum em muitos programas em C. A data atual pode ser usada para exibir informações para o usuário, registrar atividades ou até mesmo para fins de organização. Além disso, a obtenção da data atual é uma habilidade fundamental para qualquer programador iniciante em C.

## Como fazer isso em C?

Em C, a biblioteca padrão "time.h" fornece várias funções para trabalhar com datas e horas. Para obter a data atual, vamos utilizar a função "time()", que retorna o número de segundos desde 1º de janeiro de 1970, conhecido como Tempo Unix. Segue um exemplo de código:

```
#include <stdio.h>
#include <time.h>

int main() {
    // Obtém a data e hora atual
    time_t t = time(NULL);

    // Converte para uma estrutura de data
    struct tm *data = localtime(&t);

    // Imprime a data atual no formato desejado
    printf("A data atual é %02d/%02d/%d\n", data->tm_mday, data->tm_mon+1, data->tm_year+1900);
    return 0;
}
```
A saída do código acima será: "A data atual é 31/01/2021", por exemplo.

## Aprofundando

Além da função "time()", a biblioteca "time.h" possui outras funções úteis para trabalhar com datas e horas, como "mktime()" e "strftime()". Também é possível manipular os campos individuais de uma estrutura de data, por exemplo, para alterar a data atual para um dia específico. É importante lembrar que a função "localtime()" retorna a data e hora no fuso horário local do sistema.

## Veja também

- [Tutorial em português sobre a biblioteca time.h em C](https://www.devmedia.com.br/trabalhando-com-data-e-hora-em-linguagem-c/25576)
- [Documentação oficial da biblioteca time.h em C](https://www.cplusplus.com/reference/ctime/)