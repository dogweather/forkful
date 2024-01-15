---
title:                "Lendo argumentos da linha de comando"
html_title:           "Arduino: Lendo argumentos da linha de comando"
simple_title:         "Lendo argumentos da linha de comando"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que ler argumentos da linha de comando?

Quando você programa em Arduino, é importante entender como lidar com argumentos da linha de comando, pois isso permite que você personalize e interaja com seu código de forma mais eficaz. Além disso, aprender a ler argumentos da linha de comando pode expandir suas habilidades de programação e torná-lo um desenvolvedor mais versátil.

## Como fazer isso

Para ler argumentos da linha de comando em Arduino, você precisará usar o objeto `Serial`. Aqui está um exemplo de código que lerá os argumentos digitados pelo usuário na porta serial:

```
Arduino void setup() {
  // Inicializa a porta serial
  Serial.begin(9600);

  // Aguarda a conexão com a porta serial
  while (!Serial) {
    ;
  }
}

void loop() {
  // Aguarda a entrada do usuário
  while (Serial.available()) {
    // Lê o próximo caractere
    char c = Serial.read();

    // Imprime o caractere na tela do monitor serial
    Serial.print(c);
  }
}
```

Agora, se o usuário digitar algo na porta serial, como "Hello World!", o código acima irá imprimir "Hello World!" na tela do monitor serial. Experimente digitando diferentes argumentos e veja como o código lida com eles.

## Aprofundando

Existem alguns conceitos importantes a serem entendidos ao ler argumentos da linha de comando em Arduino. Primeiro, você precisa estar ciente do tamanho máximo do buffer da porta serial, que é de 128 bytes. Isso significa que o máximo de caracteres que você pode enviar na porta serial de uma vez é 128. Além disso, é importante lembrar que os argumentos fornecidos pelo usuário são considerados como uma cadeia de caracteres, e você pode usar funções como `strcmp` para compará-los com outras cadeias de caracteres.

## Veja também

- [Guia oficial do Arduino para comunicação serial](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [Tutorial sobre como ler entrada do usuário em Arduino](https://www.arduino.cc/en/Tutorial/BuiltInExamples/ReadASCIIString)