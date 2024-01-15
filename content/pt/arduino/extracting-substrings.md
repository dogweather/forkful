---
title:                "Extraindo subcadeias"
html_title:           "Arduino: Extraindo subcadeias"
simple_title:         "Extraindo subcadeias"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que

O Arduino é um microcontrolador amplamente utilizado em projetos makers e DIY (faça você mesmo). E, em alguns desses projetos, pode ser necessário manipular strings, ou seja, sequências de caracteres. Entre essas manipulações, pode ser necessário extrair pedaços específicos de uma string, o que torna a extração de substrings uma habilidade útil a ser dominada por programadores de Arduino.

## Como fazer

Para extrair substrings em Arduino, você pode usar a função "substring()". Essa função recebe dois argumentos: o índice do primeiro caractere da substring e o número de caracteres a serem extraídos. Por exemplo, se você tiver a string "Arduino é incrível", pode usar o seguinte código para extrair a substring "incrível":

```
String str = "Arduino é incrível";
String substr = str.substring(10, 8);
// substr agora contém "incrível"
```

Você também pode usar a função "substring()" com variáveis e até mesmo combinar com outras funções de string, como "indexOf()" e "length()". Algumas dicas para se lembrar ao trabalhar com extração de substrings:

- O primeiro caractere de uma string tem índice "0";
- O número de caracteres inclui o primeiro caractere, mas não inclui o último;
- Se você omitir o segundo argumento, a substring abrange até o final da string original.

E aqui está uma lista com alguns exemplos de saída usando "substring()":

| String original         | Índice  | Número de caracteres | Substring resultante |
|-------------------------|---------|----------------------|----------------------|
| "Arduino é incrível"    | 7       | 9                    | "é incrível"         |
| "Lorem ipsum dolor sit" | 0       | 5                    | "Lorem"              |
| "Maçã, laranja, banana" | 9       | 10                   | "laranja, b"         |
| "1234567890"            | 2       | 4                    | "3456"               |

## Mergulho profundo

A função "substring()" é útil para casos simples de extração de substrings. No entanto, quando as strings são mais complexas ou há várias substrings que precisam ser extraídas, pode ser necessário recorrer a outras técnicas. Por exemplo, você pode usar a função "StringTokenizer" para dividir uma string em vários pedaços, usando um caractere de separação como referência.

Além disso, se você tiver uma string em formato de data, pode usar a biblioteca "TimeLib.h" para extrair o dia, mês, ano ou até mesmo informações sobre o fuso horário. Há também a função "strstr()", que permite encontrar uma substring dentro de outra string.

A extração de substrings pode ser útil em várias situações de programação em Arduino, como trabalhar com sensores e exibir informações em displays. Portanto, é importante entender as diferentes técnicas e funções disponíveis para manipular strings.

## Veja também

- [Referência da função "substring()"](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/)
- [Exemplo de uso da função "substring()"](https://create.arduino.cc/projecthub/Arduino_Genuino/string-functions-substring-1fc2c2?ref=search&ref_id=substring&offset=2)
- [Tutorial sobre a função "StringTokenizer"](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/stringtokenizer/)