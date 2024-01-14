---
title:                "Arduino: Escrevendo para o erro padrão"
simple_title:         "Escrevendo para o erro padrão"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que escrever para o erro padrão?

Escrever para o erro padrão pode ser uma ferramenta útil para programadores Arduino. Quando há um erro no código, uma mensagem de erro é enviada para o erro padrão, permitindo que o problema seja identificado e resolvido mais facilmente. Neste artigo, vamos explorar como usar essa funcionalidade para melhorar nossos projetos Arduino.

## Como fazer:

Para escrever para o erro padrão no Arduino, usamos a função `Serial.println()`. Isso enviará uma mensagem de erro para a porta serial USB, que pode ser lida por um programa no computador. Vamos ver um exemplo simples:

```
Arduino.setup() {
  Serial.begin(9600); //Inicia a comunicação serial com taxa de 9600 baud
}

Arduino.loop() {
  int valor = analogRead(A0); //Lê o valor do pino analógico A0
  if(valor < 200) { //Se o valor for menor que 200, há um problema
    Serial.println("Erro: Sensor de luz não está funcionando corretamente!"); //Envia mensagem de erro para o erro padrão
  }
}
```

Neste exemplo, nosso código verifica o valor do pino analógico A0 e, se for menor que 200, exibe uma mensagem de erro no erro padrão. Podemos usar esta funcionalidade para verificar sensores, atuadores ou qualquer outra parte do nosso projeto.

## Mergulho profundo:

Você pode estar se perguntando: por que não apenas exibir a mensagem de erro no monitor serial? A resposta é que nem sempre temos um monitor serial conectado ao Arduino, por isso é útil ter a opção de mostrar erros em outro lugar, como no computador. Além disso, a função `Serial.println()` é útil não apenas para exibir mensagens de erro, mas também para enviar informações importantes sobre nosso projeto para o computador.

Além disso, a função `Serial.println()` é apenas uma das muitas funções de comunicação serial disponíveis no Arduino. Existem outras funções, como `Serial.write()` e `Serial.print()`, que nos dão mais controle sobre como os dados são enviados. Se você quiser se aprofundar ainda mais no assunto, pode conferir o [Manual de Referência da Comunicação Serial do Arduino](https://www.arduino.cc/reference/en/language/functions/communication/serial/) para obter mais informações.

## Veja também:

- [Documentação oficial do Arduino sobre comunicação serial](https://www.arduino.cc/en/Tutorial/Serial) (em inglês)
- [Vídeo tutorial sobre comunicação serial no Arduino](https://www.youtube.com/watch?v=Ve4PCnNcdiI) (em português)
- [Outras funções de comunicação serial no Arduino](https://web.iit.edu/sites/web/files/departments/academic-affairs/academic-resource-center/pdfs/Arduino-Tutorial-6.pdf) (em inglês)