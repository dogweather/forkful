---
title:                "Concatenando strings"
date:                  2024-01-20T17:33:58.170392-07:00
model:                 gpt-4-1106-preview
simple_title:         "Concatenando strings"

category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Concatenar strings é juntar duas ou mais sequências de caracteres para formar uma nova. Programadores fazem isso para montar mensagens, dados ou comandos de forma dinâmica.

## Como Fazer:
```Arduino
String primeiroNome = "João";
String sobrenome = "Silva";
String nomeCompleto = primeiroNome + " " + sobrenome; 

void setup() {
  Serial.begin(9600);
}

void loop() {
  Serial.println(nomeCompleto); // Saída: João Silva
  delay(1000); // Espera 1 segundo antes de repetir
}
```

## Mergulho Profundo
Concatenar strings é um conceito que existe desde os primórdios da programação. No contexto do Arduino, é importante ser eficiente com memória, então usar a classe `String` pode ser custoso para programas maiores. Alternativas como `strcat()` da biblioteca `cstring` podem ser usadas com arrays de char para otimizar o uso de memória. Cuidado com o buffer overflow!

## Veja Também
- Documentação do Arduino sobre strings: https://www.arduino.cc/reference/en/language/variables/data-types/string/
- Tutorial sobre gerenciamento de memória no Arduino: https://www.arduino.cc/en/Tutorial/Memory
- Fórum Arduino com discussões sobre strings: http://forum.arduino.cc/index.php?board=4.0
