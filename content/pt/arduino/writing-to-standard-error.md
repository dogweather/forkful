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

## O Que & Por Que?
Escrever no erro padrão (stderr) é o processo de exibir mensagens de erro em um canal separado da saída padrão (stdout). Programadores usam isso para reportar erros sem interferir com a saída regular do programa.

## Como Fazer:
Na programação Arduino, não existe um stderr dedicado como em sistemas operacionais completos. Em vez disso, usamos Serial.print ou Serial.println para depuração e envio de erros. Abaixo está um exemplo de como você poderia implementar algo como stderr:

```cpp
void setup() {
  Serial.begin(9600);
}

void loop() {
  // Simulando um erro
  if (digitalRead(2) == LOW) {
    // simulando stderr
    Serial.println("Erro: Botão pressionado!");
  } else {
    // simulando stdout
    Serial.println("Tudo funcionando corretamente.");
  }
  delay(1000);
}
```

Saída (considere que o pino 2 está conectado a um botão):
```
Tudo funcionando corretamente.
Erro: Botão pressionado!
```
## Mergulho Profundo
No mundo Arduino, o conceito de stderr é simplificado devido ao ambiente de hardware e software. Historicamente, em sistemas Unix, o stderr é um fluxo independente que programas usam para enviar mensagens de erro enquanto o stdout é para saídas regulares. No Arduino, utilizamos a mesma porta Serial para ambos, mas é uma prática comum separar mensagens de erro de outras saídas para facilitar a depuração. No entanto, a implementação física do stderr não existe no ambiente do Arduino.

Alternativas:
- Usar LEDs ou displays para indicar tipos diferentes de erro.
- Implementar uma biblioteca que simula diferentes níveis de log.

Detalhes de implementação:
- Se necessário, criar funções customizadas para erro que incluem informações como o timestamp, tipo do erro, etc.

## Veja Também
- Documentação da Arduino Serial Library: https://www.arduino.cc/reference/en/language/functions/communication/serial/
- Artigo sobre boas práticas de depuração em Arduino: https://www.arduino.cc/en/Guide/Troubleshooting
- Tutorial sobre comunicação Serial com Arduino: https://www.arduino.cc/en/Tutorial/BuiltInExamples/SerialEvent