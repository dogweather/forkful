---
title:                "Calculando uma data no futuro ou passado"
html_title:           "C: Calculando uma data no futuro ou passado"
simple_title:         "Calculando uma data no futuro ou passado"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Calculando uma data no futuro ou passado envolve manipular datas para obter um momento específico anterior ou subsequente à data atual. Programadores fazem isso para programar eventos, como lembretes, ou para calcular a diferença entre duas datas.

## Como fazer:

Podemos usar a função `mktime` e a estrutura `tm` para calcular uma data futura. 

```C
#include <stdio.h>
#include <time.h>

int main() {
    struct tm str_tm = {0};
    time_t tempo_atual, futuro_tempo;

    // Se pegar tempo atual
    tempo_atual = time(NULL);

    // Copiar para a estrutura str_tm
    str_tm = *localtime(&tempo_atual);

    // Adicionar dias
    str_tm.tm_mday += 10;

    // Converter de volta para time_t
    futuro_tempo = mktime(&str_tm);

    printf("Data atual: %s", ctime(&tempo_atual));
    printf("Data futura: %s", ctime(&futuro_tempo));

    return 0;
}
```

Saída do código:

```
Data atual: Fri Jan 21 07:39:16 2022
Data futura: Mon Jan 31 07:39:16 2022
```

## Deep Dive:

Calculando uma data no futuro ou no passado é uma tarefa comum na programação desde o início dos tempos (não, não é uma piada de tempo!). As linguagens de programação modernas, como C, têm bibliotecas abrangentes que facilitam a manipulação de datas.

Uma alternativa ao método acima envolveria a criação de sua própria função para adicionar ou subtrair dias a uma data, levando em conta a variação no número de dias em cada mês e ano bissexto.

Quanto à implementação da função `mktime`, ela pega uma estrutura `tm`, converte-a de volta para `time_t`, normaliza a estrutura `tm` e corrige qualquer valor fora do intervalo. Isso faz da `mktime` uma função útil para manipular datas.

## Veja Também:

- [`time.h` Reference](http://www.cplusplus.com/reference/ctime/)