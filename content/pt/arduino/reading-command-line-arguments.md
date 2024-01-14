---
title:                "Arduino: Lendo argumentos da linha de comando"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que ler argumentos da linha de comando no Arduino

Ler argumentos da linha de comando no Arduino pode ser extremamente útil para uma melhor interação e controle de seu projeto. Com a capacidade de receber e interpretar esses argumentos, você pode adicionar mais funcionalidades ao seu código, tornando-o mais dinâmico e adaptável. Além disso, essa prática pode ajudar a simplificar o processo de comunicação entre diferentes dispositivos e sistemas.

## Como fazer

Para começar a ler argumentos da linha de comando no Arduino, siga os seguintes passos:

1. Declaração de variáveis:
Antes de qualquer coisa, é necessário declarar uma variável do tipo "char" para armazenar o argumento recebido pela linha de comando. Por exemplo: "char argumento;".

2. Inicialização da porta serial:
Antes de utilizar a porta serial para receber os dados da linha de comando, é necessário inicializá-la com a função "Serial.begin(baudrate);", onde "baudrate" representa a velocidade de comunicação.

3. Leitura de argumentos:
Para receber os argumentos, é preciso utilizar a função "Serial.readStringUntil(caractere);" dentro da função "loop()" do seu código. O parâmetro "caractere" representa o delimitador que será utilizado para a leitura da string recebida. Por exemplo: "Serial.readStringUntil('\n');" para ler até que seja recebido o caractere de quebra de linha.

4. Utilização dos argumentos:
Posteriormente à leitura da string, você pode utilizar a variável declarada anteriormente para armazenar esse argumento e utilizá-lo em seu código como achar necessário.

```Arduino
char argumento; // declaração da variável
Serial.begin(9600); // inicialização da porta serial
void loop(){ // função loop
  argumento = Serial.readStringUntil('\n'); // leitura do argumento
  // utilizar o argumento recebido em seu código
}
```

## Aprofundando-se

Além dos passos básicos descritos acima, é importante lembrar de algumas coisas ao lidar com argumentos da linha de comando no Arduino:

- É necessário utilizar um delimitador para a leitura da string, caso contrário, o Arduino irá continuar lendo até que a memória esteja cheia ou o tempo limite da função expire. Isso pode causar erros ou travamentos no código.

- É importante estar atento à velocidade de comunicação ao utilizar a porta serial, pois se essa velocidade não estiver de acordo com a do dispositivo que está enviando os argumentos, a comunicação pode falhar.

## Veja também

- Documentação oficial sobre a função "Serial.readStringUntil()": https://www.arduino.cc/reference/pt/language/functions/communication/serial/readstringuntil/
- Exemplo prático de como ler argumentos da linha de comando no Arduino: https://create.arduino.cc/projecthub/pcarvalho/read-serial-data-b209cd
- Vídeo tutorial explicando como lidar com argumentos da linha de comando no Arduino: https://www.youtube.com/watch?v=xk-KcTCLLDg