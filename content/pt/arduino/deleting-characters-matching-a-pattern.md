---
title:    "Arduino: Excluindo caracteres que correspondem a um padrão"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

Olá leitores do Arduino!

Hoje vamos falar sobre como deletar caracteres que seguem um determinado padrão em um programa de Arduino. Você pode estar se perguntando por que alguém iria querer fazer isso - a resposta é que pode ser útil para limpar dados ou formatar entradas em projetos eletrônicos.

## Por que

Agora que você sabe o *porquê* deste processo, vamos mergulhar no *como* fazê-lo no seu programa de Arduino.

## Como Fazer

Para deletar caracteres que seguem um padrão em um programa de Arduino, você vai precisar usar a função `removeChars()` do tipo `String`. Essa função irá buscar por todos os caracteres que correspondem ao padrão especificado e os deletará da sua string original.

```Arduino
String minhaString = "Olá, Arduino!";
minhaString.removeChars("lá"); //remove os caracteres "lá"
Serial.println(minhaString); //Saída: O, Arduino!
```

## Deep Dive

A função `removeChars()` utiliza o método `substring()` para remover os caracteres especificados da string original. Este método utiliza dois parâmetros - o primeiro é a posição inicial do trecho de caracteres a ser removido, e o segundo é o comprimento desse trecho. O método `substring()` retorna um novo `String` sem o trecho especificado, e é isso que a função `removeChars()` usa para deletar os caracteres do seu programa.

## Veja também

Aqui estão alguns links para mais informações sobre o uso da função `removeChars()` e outras funções úteis em projetos de Arduino:

- [Referência da função removeChars()](https://www.arduino.cc/reference/en/language/variables/string/functions/removechars/)
- [Tutorial de String no Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [Outras funções String do Arduino](https://www.arduino.cc/reference/en/language/functions/communication/string-functions/)