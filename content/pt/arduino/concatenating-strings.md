---
title:                "Arduino: Concatenação de strings"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

# Por que utilizar a concatenação de strings no Arduino
Se você está trabalhando com strings no seu projeto do Arduino e precisa juntar várias delas para formar uma mensagem ou dado específico, a concatenação é a melhor opção. Ela é uma forma de combinar strings de forma eficiente e pode ser muito útil para criar outputs personalizados e dinâmicos. 

## Como utilizar a concatenação de strings no Arduino
Para concatenar strings no Arduino, você precisará primeiro definir as variáveis que irão armazenar as strings que serão combinadas. Em seguida, você pode utilizar o operador "+" para juntar as variáveis e formar uma nova string. Veja um exemplo de código abaixo:

```Arduino
String nome = "José";
String sobrenome = "Silva";

String nomeCompleto = nome + " " + sobrenome;
```
O código acima irá atribuir o valor "José Silva" à variável "nomeCompleto". Você pode então utilizar essa nova string para exibir uma mensagem ou realizar outras operações.

## Aprofundando na concatenação de strings
Existem algumas coisas que é importante saber sobre a concatenação de strings no Arduino. Primeiramente, é importante notar que é possível combinar mais de duas strings utilizando o operador "+". Por exemplo:

```Arduino
String palavra1 = "Olá";
String palavra2 = "mundo";
String palavra3 = "!";

String saudacao = palavra1 + " " + palavra2 + palavra3;
```

Também é importante lembrar que apenas as strings podem ser concatenadas dessa forma. Se você tentar combinar um número inteiro (int) ou um caractere (char) com uma string, você pode obter um resultado indesejado. Para isso, é necessário converter esses tipos de dados em strings antes de concatená-los.

## Veja Também
- [Documentação oficial do Arduino sobre a string concatenation](https://www.arduino.cc/reference/pt/language/variables/data-types/string/)
- [Tutorial sobre como utilizar a concatenação de strings no Arduino](https://create.arduino.cc/projecthub/Arduino_Genuino/strings-df38c3)