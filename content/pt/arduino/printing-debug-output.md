---
title:                "Arduino: Saida de depuração de impressão"
simple_title:         "Saida de depuração de impressão"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que?

A impressão de saída de depuração (debug output) é uma técnica essencial na programação Arduino para monitorar o processo de execução do código e identificar possíveis problemas ou erros. Ela permite visualizar o valor das variáveis, status dos pins e mensagens de feedback no monitor serial, facilitando o desenvolvimento e a identificação de problemas no código.

## Como Fazer:
Para imprimir saída de depuração no Arduino, siga os seguintes passos:

1. Abra a IDE do Arduino e conecte o seu dispositivo ao computador.
2. Certifique-se de selecionar o dispositivo correto no menu "Ferramentas" e a porta serial correta no submenu "Porta".
3. Adicione o comando "Serial.begin()" no início do código para iniciar a comunicação serial.
4. Para imprimir uma mensagem no monitor serial, utilize o comando "Serial.println()" ou "Serial.print()" seguido da mensagem que deseja exibir.
5. Ao executar o código, a saída de depuração será exibida no monitor serial.

Veja um exemplo de código abaixo:

```Arduino
void setup(){
  Serial.begin(9600); // inicia a comunicação serial na taxa de 9600 bps
}

void loop(){
  int sensor = analogRead(A0); // lê o valor do sensor conectado ao pin A0
  Serial.print("Valor do sensor: "); // imprime uma mensagem
  Serial.println(sensor); // imprime o valor do sensor
  delay(100); // aguarda 100ms
}
```

Ao executar esse código, você verá a mensagem "Valor do sensor: [valor do sensor]" sendo impressa no monitor serial a cada 100 milissegundos.

## Profundidade:

Existem algumas formas de aprimorar a impressão de saída de depuração em seus projetos Arduino. Dentre elas, podemos citar:

- Utilizar diferentes tipos de dados, como "Serial.print()" para imprimir um valor formatado ou "Serial.write()" para imprimir valores binários.
- Utilizar tags HTML para formatar a saída da impressão no monitor serial.
- Utilizar o recurso de "print concatenation" para imprimir múltiplas variáveis em uma única linha.
- Utilizar bibliotecas de debug como a "SerialDebug" para facilitar o processo de impressão de saída de depuração.
- Utilizar a função "Serial.flush()" para esvaziar o buffer de saída e garantir que todas as mensagens sejam exibidas corretamente.

## Veja também:

Confira os seguintes links para mais informações sobre a impressão de saída de depuração no Arduino:

- [Documentação oficial do Arduino sobre a comunicação serial](https://www.arduino.cc/reference/pt/language/functions/communication/serial/)
- [Tutorial sobre a impressão de saída de depuração no Arduino](https://www.filipeflop.com/blog/imprimindo-dados-de-depuracao-no-monitor-serial-do-arduino/)
- [Biblioteca SerialDebug para facilitar a impressão de saída de depuração](https://github.com/JoaoLopesF/serial-debug)
- [Exemplo de uso de diferentes tipos de dados na impressão de saída de depuração](https://www.arduino.cc/reference/pt/language/variables/data-types/)