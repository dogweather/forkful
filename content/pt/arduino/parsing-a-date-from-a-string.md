---
title:                "Analisando uma data a partir de uma string"
html_title:           "PowerShell: Analisando uma data a partir de uma string"
simple_title:         "Analisando uma data a partir de uma string"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## O Quê & Porquê?

Analisar a data de uma string significa extrair uma data válida de um conjunto de caracteres. Os programadores fazem isso para manipular e interagir com datas de forma eficiente.

## Como Fazer:

Para interpretar uma data de uma string no Arduino, vamos usar a função `strtok()`. Para obter a data de uma string no formato "dd-mm-yyyy":

```Arduino
char data[] = "21-03-2020";
char delimitador[] = "-";

char *dia = strtok(data, delimitador);
char *mes = strtok(NULL, delimitador);
char *ano = strtok(NULL, delimitador);

Serial.println(dia);
Serial.println(mes);
Serial.println(ano);
```

A saída será:

```Arduino
21
03
2020
```

Aqui, `strtok()` está dividindo a data em dia, mês e ano usando '-' como delimitador.

## Um Olhar Mais Profundo 

O tratamento de datas sempre foi uma parte vital da computação. Desde o início do Unix nos anos 70, a hora e a data foram capturadas como segundos passados desde 1 de janeiro de 1970, uma prática que ainda está em voga hoje, conhecida como Unix Time. A interpretação da data a partir de uma string é um pequeno fragmento dessa funcionalidade global.

Em termos de alternativas, em outras linguagens de programação como o Python, existem módulos de análise de data integrados como `dateutil.parser`. No entanto, devido à natureza simples e direta da linguagem C no Arduino, geralmente recorremos a `strtok()`.

Um aspecto crucial a notar é que o `strtok()` altera a string original, então se você precisa da string original intacta, faça uma cópia antes de usar `strtok()`.

## Veja Mais

Para saber mais sobre o `strtok()`, consulte: https://www.cplusplus.com/reference/cstring/strtok/

Para entender mais sobre o Unix Time, confira: https://www.unixtimestamp.com/

Para um aprofundamento na análise de datas e horas, você pode querer verificar: https://www.arduino.cc/reference/en/libraries/rtclib/