---
title:                "Análise de uma data de uma string."
html_title:           "Arduino: Análise de uma data de uma string."
simple_title:         "Análise de uma data de uma string."
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## O que é e por que fazer? 
Analisar uma data em uma string significa extrair informações específicas como dia, mês e ano de uma cadeia de caracteres. Isso é frequentemente feito por programadores para converter informações de data em algo que o código possa manipular e utilizar facilmente para realizar diferentes tarefas.

## Como fazer: 
```Arduino
  String data = "12/04/2020"; // Definindo uma string com a data que queremos analisar
  int dia, mes, ano; // Variáveis para armazenar os valores separados
  
  dia = data.substring(0,2).toInt(); // Usando a função "substring" para obter os dois primeiros caracteres da string (dia)
  mes = data.substring(3,5).toInt(); // O mesmo para os caracteres do meio (mês)
  ano = data.substring(6,10).toInt(); // E para os últimos (ano)
  
  Serial.println("Data analisada:");
  Serial.print("Dia: ");
  Serial.println(dia);
  Serial.print("Mês: ");
  Serial.println(mes);
  Serial.print("Ano: ");
  Serial.println(ano);
```

## Mergulho profundo:
Analisar datas em strings é uma tarefa comum em programação, já que muitas informações são armazenadas nesse formato. Existem várias maneiras de realizar essa tarefa, dependendo da linguagem de programação que você está usando. No Arduino, a função "substring" é utilizada para extrair partes específicas de uma string, mas outras funções também podem ser utilizadas para realizar essa tarefa. É importante ter cuidado ao definir o formato da data na string para que a análise seja feita corretamente.

## Veja também:
- [Tutorial: manipulando datas em strings no Arduino](https://www.arduino.cc/en/Tutorial/StringSubstring)
- [Documentação da função "substring" do Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/tostring/)