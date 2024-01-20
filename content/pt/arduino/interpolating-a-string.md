---
title:                "Interpolando uma string"
html_title:           "Java: Interpolando uma string"
simple_title:         "Interpolando uma string"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Interpolação de Strings no Arduino (Versão Recente)

## O Que & Por Quê?

A interpolação de strings é uma maneira de inserir variáveis diretamente em uma string. Os programadores fazem isso para facilitar a manipulação de strings e melhorar a legibilidade do código.

## Como Fazer:

Aqui está um exemplo de como interpolar uma string no Arduino:

```Arduino
String nome = "João";
String saudacao = "Olá, " + nome + "!";
Serial.println(saudacao);
```

O resultado da saída seria:

```Arduino
Olá, João!
```

## Mergulho Profundo

Historicamente, A interpolação de strings não era suportada diretamente no C e C++ e os programadores tinham que usar a função `sprintf()`. O Arduino, que usa uma forma de C++, introduziu a classe `String` que tem muitas funções úteis, incluindo a sobrecarga do operador + para concatenação, permitindo a interpolação de strings.

As alternativas são para usar a função `snprintf()` que é mais segura do que `sprintf()` porque verifica o comprimento da string, ou a função `concat()` da classe `String` do Arduino.

Os detalhes da implementação são que o operador + para strings foi sobrecarregado para permitir a concatenação e, consequentemente, a interpolação de strings.

## Ver Também

- Visite a [Documentação Oficial do Arduino](https://www.arduino.cc/reference/en/) para aprender mais sobre a classe String e outras funcionalidades.
- Leia sobre a [Função Sprintf em C++](https://www.cplusplus.com/reference/cstdio/sprintf/) para aprofundar no uso dessa alternativa de concatenação.