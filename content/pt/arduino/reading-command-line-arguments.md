---
title:    "Arduino: Lendo argumentos da linha de comando"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Por que ler argumentos de linha de comando em Arduino?

Quando se trabalha com Arduino, é importante entender como os argumentos de linha de comando funcionam, pois isso pode facilitar muito o processo de programação e depuração. Além disso, a leitura de argumentos de linha de comando pode ser útil em situações em que o usuário precisa inserir dados específicos para fazer o dispositivo funcionar corretamente.

## Como fazer a leitura de argumentos de linha de comando em Arduino

Para ler argumentos de linha de comando em Arduino, você pode seguir os seguintes passos:

1. Declare as variáveis ​​que você deseja preencher com os argumentos de linha de comando:
    ```arduino
    int num1, num2, num3;
    ```

2. Na função "setup()", use o comando "Serial.begin()" para iniciar a comunicação serial e definir a velocidade para 9600 baud:
    ```arduino
    Serial.begin(9600);
    ```

3. Na função "loop()", use o comando "Serial.available()" para verificar se há dados disponíveis na porta serial. Se houver, use o comando "Serial.parseInt()" para ler o próximo argumento e armazená-lo na variável correspondente:
    ```arduino
    if (Serial.available() > 0) {
      num1 = Serial.parseInt();
      num2 = Serial.parseInt();
      num3 = Serial.parseInt();
    }
    ```

4. Para enviar os argumentos de linha de comando para o Arduino, abra o monitor serial e insira os valores separados por espaços e clique no botão "Enviar":
    ```
    10 20 30
    ```
    Isso enviará os valores 10, 20 e 30 para as variáveis "num1", "num2" e "num3", respectivamente.

## Mergulho profundo na leitura de argumentos de linha de comando em Arduino

Ao ler argumentos de linha de comando em Arduino, é importante ter em mente que eles são sempre lidos como valores inteiros. Se o valor inserido pelo usuário não puder ser convertido para um inteiro, o valor da variável será definido como 0. Além disso, para enviar argumentos de linha de comando para o Arduino, é necessário utilizar os comandos "Serial.parseInt()" ou "Serial.parseFloat()" para ler os valores corretamente.

## Veja também

Aqui estão alguns links úteis para saber mais sobre a leitura de argumentos de linha de comando em Arduino:

- [Documentação oficial do Arduino](https://www.arduino.cc/reference/en/language/functions/communication/serial/parseint/)
- [Tutorial prático da Adafruit](https://learn.adafruit.com/serial-input-basics/docs)
- [Vídeo tutorial do canal Fritzing](https://www.youtube.com/watch?v=tRbrHk6EpqQ)