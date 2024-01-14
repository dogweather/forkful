---
title:                "Arduino: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que converter uma string para letras minúsculas?

Ao escrever códigos em Arduino, muitas vezes é necessário manipular strings, que são sequências de caracteres. Em alguns casos, é necessário converter essa string para letras minúsculas. Isso pode ser útil para fins de comparação de strings, formatação de entrada de dados ou qualquer outro caso em que é necessário que os caracteres estejam em letras minúsculas. Portanto, aprender a converter uma string para letras minúsculas é um conhecimento importante para um programador de Arduino.

## Como fazer isso:

Para converter uma string para letras minúsculas no Arduino, precisamos usar a função `toLowerCase()`. Esta função aceita uma string como parâmetro e retorna uma nova string com todas as letras em letras minúsculas. Aqui está um exemplo de código e sua saída:

```Arduino
String stringExemplo = "ARDUINO";
String stringConvertida = stringExemplo.toLowerCase();
Serial.println(stringConvertida); // Saída: arduino
```

Como você pode ver, a string foi convertida com sucesso para letras minúsculas.

## Profundidade:

Agora, vamos dar uma olhada mais aprofundada em como a função `toLowerCase()` funciona por trás dos bastidores. Quando invocamos a função, um novo objeto de string é criado na memória. Em seguida, percorremos cada caractere da string original e convertemos cada caractere para sua versão em minúsculas usando a tabela ASCII. Em seguida, concatenamos esses caracteres e retornamos a nova string convertida. Isso é feito internamente pela função `toLowerCase()`, então não precisamos nos preocupar com isso.

Além disso, é importante lembrar que a função `toLowerCase()` só converte caracteres ASCII. Caracteres que não têm um equivalente em minúsculas, como caracteres acentuados, não serão convertidos e continuarão em sua forma original.

## Veja Também:

Aqui estão alguns links úteis para aprender mais sobre a função `toLowerCase()` e outras funções de manipulação de strings no Arduino:

- [Referência oficial da função toLowerCase()](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/tolowercase/)
- [Tutorial sobre o uso de strings no Arduino](https://www.tutorialspoint.com/arduino/arduino_strings.htm)
- [Tutorial sobre a tabela ASCII](https://www.ascii-code.com/)

Esperamos que este artigo tenha ajudado você a entender melhor como converter uma string para letras minúsculas no Arduino. Agora você pode usá-lo em seus projetos e scripts para manipular strings de forma eficiente. Divirta-se codificando!