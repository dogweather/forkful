---
title:                "Arduino: Trabalhando com json"
simple_title:         "Trabalhando com json"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/working-with-json.md"
---

{{< edit_this_page >}}

## Por que trabalhar com JSON no Arduino

Se você está interessado em controlar projetos e dispositivos com o Arduino, uma das habilidades importantes para aprender é trabalhar com JSON. JSON (Javascript Object Notation) é uma forma compacta de armazenar e transmitir dados em formato de texto, tornando-se uma escolha popular para muitas aplicações. Ao aprender a trabalhar com JSON no Arduino, você poderá enviar e receber dados de forma eficiente, permitindo a comunicação com outros dispositivos e a criação de projetos interessantes.

## Como Trabalhar com JSON no Arduino

### Passo 1: Baixe e Instale a Biblioteca ArduinoJSON

Para começar a trabalhar com JSON no Arduino, é necessário ter a biblioteca ArduinoJSON instalada em seu IDE. Certifique-se de ter a versão mais recente da biblioteca e de seguir os passos para sua instalação corretamente.

### Passo 2: Criando um Objeto JSON

Para criar um objeto JSON, usamos o construtor `StaticJsonDocument` e passamos o tamanho do buffer como parâmetro. Por exemplo, para criar um objeto JSON com tamanho de buffer de 256 bytes, usamos o seguinte código:

```Arduino
StaticJsonDocument<256> doc;
```

### Passo 3: Adicionando Dados ao Objeto JSON

Após criar o objeto JSON, podemos adicionar dados a ele usando o método `set()` e passando o nome do campo e o valor desejado. Por exemplo, para adicionar o valor "25" ao campo "temperatura", usamos o seguinte código:

```Arduino
doc.set("temperatura", 25);
```

### Passo 4: Serializando o Objeto JSON

Finalmente, para enviar o objeto JSON criado, precisamos serializá-lo usando `serializeJson()` e passando o objeto como parâmetro. Nesse exemplo, estamos enviando o objeto JSON por meio da porta serial:

```Arduino
serializeJson(doc, Serial);
```

## Mergulho Profundo em JSON

Há muitos elementos e funções interessantes que podem ser explorados quando se trabalha com JSON no Arduino. Alguns recursos avançados incluem:

- Acesso a campos específicos em objetos JSON usando o operador `[]`.
- Armazenamento de objetos JSON em memória Flash em vez de memória RAM.
- Aninhamento de objetos e arrays JSON para criar estruturas de dados mais complexas.

Ao explorar esses recursos, você poderá aprimorar suas habilidades em trabalhar com JSON e criar projetos ainda mais interessantes com o Arduino.

## Veja Também

- [Documentação da biblioteca ArduinoJSON](https://arduinojson.org/)
- [Exemplo de projeto com JSON no Arduino](https://create.arduino.cc/projecthub/ian1991india/sending-complex-data-structures-via-serial-using-json-a2b088)
- [Tutorial em vídeo sobre trabalhar com JSON no Arduino](https://www.youtube.com/watch?v=hIPOUVlw0I4)