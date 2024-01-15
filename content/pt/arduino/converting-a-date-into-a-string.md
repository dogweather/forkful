---
title:                "Convertendo uma data em uma string."
html_title:           "Arduino: Convertendo uma data em uma string."
simple_title:         "Convertendo uma data em uma string."
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Porque
Você pode querer converter uma data em uma string para poder exibi-la em um display ou enviá-la por meio de comunicação serial. Isso é útil para fins de registro ou para fazer com que o sistema exiba a data em um formato mais legível.

## Como Fazer
Para converter uma data em uma string no Arduino, primeiro você precisa incluir a biblioteca "Time.h". Em seguida, defina uma variável do tipo "tmElements_t" e preencha seus campos com a data e hora desejadas. Finalmente, use a função "makeTime()" para converter essa estrutura em um valor de tempo UNIX e, em seguida, passe esse valor para a função "printFormatted()" junto com um formato de string.

```
Arduino #include <Time.h>

tmElements_t dataHora; // Variável para armazenar a data e hora
dataHora.Year = 2022; // Defina o ano
dataHora.Month = 01; // Defina o mês
dataHora.Day = 01; // Defina o dia
dataHora.Hour = 12; // Defina a hora
dataHora.Minute = 00; // Defina os minutos
dataHora.Second = 00; // Defina os segundos

time_t valorTempo = makeTime(dataHora); // Converter a estrutura em um valor de tempo
String dataString = printFormatted(valorTempo, " %d/%m/%Y %H:%M:%S"); // Converter o valor de tempo em uma string com o formato desejado
Serial.println(dataString); // Imprimir a string no monitor serial
```

A saída será: 01/01/2022 12:00:00.

## Mais Detalhes
Para converter uma data em uma string, o Arduino usa a biblioteca "Time.h", que permite manipular datas e horas em formato UNIX. O formato de string é definido pela função "printFormatted()", que aceita vários argumentos, incluindo os formatos de data e hora desejados, como dia, mês, ano, hora, minuto e segundo.

É importante notar que o Arduino tem uma precisão limitada em relação ao tempo, então, ao usar a função "makeTime()", ele só pode lidar com datas a partir de 1970. Além disso, alguns formatos de data podem requerer um pequeno ajuste manual para obter a saída correta.

## Veja Também
- Documentação oficial da biblioteca "Time.h": https://www.arduino.cc/en/Reference/Time
- Tutoriais sobre gerenciamento de tempo no Arduino: https://www.arduino.cc/en/Tutorial/BuiltinExamples/Time
- Outras bibliotecas de gerenciamento de tempo para o Arduino: https://playground.arduino.cc/Code/Time/