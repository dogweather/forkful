---
title:                "Escrevendo no erro padrão"
html_title:           "Arduino: Escrevendo no erro padrão"
simple_title:         "Escrevendo no erro padrão"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que escrever para o erro padrão?

O erro padrão é uma ferramenta essencial para depurar e monitorar o comportamento do seu código Arduino. Ao escrever para o erro padrão, você pode obter informações detalhadas sobre o que está acontecendo dentro do seu programa e identificar possíveis erros rapidamente.

## Como Fazer

Para escrever para o erro padrão em seu código Arduino, siga os seguintes passos:

- Primeiro, inclua a biblioteca "Arduino.h" no início do seu código.

- Em seguida, utilize a função `Serial.begin()` para iniciar a comunicação serial com uma taxa de transmissão de dados.

- Agora, use a função `Serial.println()` para enviar uma mensagem para o erro padrão. Por exemplo: 

```Arduino
Serial.println("Mensagem de erro");
```

- Você também pode enviar variáveis para o erro padrão usando `Serial.println()` e a função `String()`. Por exemplo:

```Arduino
int temperatura = 25;
String mensagem = "A temperatura atual é: ";

Serial.println(mensagem + String(temperatura));
```

- Para visualizar as mensagens do erro padrão, abra a janela do "Monitor Serial" localizada no menu "Ferramentas" do Arduino IDE.

## Uma Profundidade de Mergulho

Ao usar a função `Serial.println()`, tenha em mente que o texto enviado para o erro padrão deve estar entre aspas duplas. Se você quiser enviar valores decimal ou hexadecimal para o erro padrão, use `Serial.println()` e `Serial.print()` respectivamente.

Além disso, você pode personalizar a taxa de transmissão de dados ao iniciar a comunicação serial com a função `Serial.begin()`. Isso pode ser útil para depurar problemas de desempenho em seu código.

## Veja Também

- [Documentação do Arduino sobre comunicação serial](https://www.arduino.cc/en/Serial/)
- [Tutorial em vídeo sobre como usar o Monitor Serial](https://www.youtube.com/watch?v=8ZjfA6wfKt4)
- [Fórum da comunidade do Arduino](https://forum.arduino.cc/index.php?board=1.0)