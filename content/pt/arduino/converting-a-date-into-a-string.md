---
title:                "Arduino: Converter uma data em uma string"
programming_language: "Arduino"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que converter uma data em uma string?

Converter uma data em uma string pode ser útil em muitos projetos de Arduino, especialmente quando se trata de exibir informações de data e hora para o usuário. Além disso, pode ser uma habilidade valiosa para adicionar valor às suas habilidades de programação em geral.

## Como fazer:

Para converter uma data em uma string, você pode usar a função ```Arduino.String()```. Primeiro, defina uma variável do tipo ```String``` para armazenar a data. Em seguida, use a função ```String()``` para converter os valores de data e hora em uma string. A seguir está um exemplo simples:

```Arduino
String data = String(dia) + "/" + String(mês) + "/" + String(ano);
```

O programa acima irá converter os valores de dia, mês e ano em uma string no formato DD/MM/AAAA. Você também pode usar essa função para formatar strings em diferentes formatos, como MM/DD/AAAA ou AAAA/MM/DD. É importante lembrar que as variáveis de data e hora devem ser do tipo ```int``` para funcionar corretamente.

## Explorando mais a fundo:

Além da função ```String()```, existem outras formas de converter data e hora em string em projetos de Arduino. Por exemplo, a biblioteca "TimeLib.h" possui funções específicas para lidar com datas e horas em formato string. Além disso, é possível usar a função ```sprintf()``` para formatar uma string com base em um modelo pré-definido.

Adicionar funcionalidades de data e hora aos seus projetos de Arduino pode ser muito útil, mas também pode ser um desafio. Por isso, sempre pesquise e experimente diferentes métodos para encontrar o que melhor se adapta às suas necessidades e habilidades como programador.

## Veja também:

- [Função String()](https://www.arduino.cc/reference/pt/language/variables/data-types/string/)
- [Biblioteca TimeLib.h](https://www.arduino.cc/reference/pt/libraries/timelib/)
- [Função sprintf()](https://www.arduino.cc/reference/en/language/functions/communication/serial/sprintf/)

Obrigado por ler este post sobre como converter uma data em string para projetos de Arduino. Esperamos que você encontre essa habilidade útil em seus futuros projetos. Boa sorte!