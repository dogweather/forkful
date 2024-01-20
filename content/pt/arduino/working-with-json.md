---
title:                "Trabalhando com json"
html_title:           "Arduino: Trabalhando com json"
simple_title:         "Trabalhando com json"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/working-with-json.md"
---

{{< edit_this_page >}}

## O que & Por quê?
Trabalhar com JSON pode ser complicado no começo, mas é uma habilidade importante para programadores dominarem. JSON é uma forma de estruturar dados para facilitar a comunicação entre diferentes aplicações. Programadores utilizam JSON para transferir dados entre sistemas ou dispositivos de forma padrão.

## Como fazer:
O Arduino possui uma biblioteca interna para trabalhar com JSON, tornando o processo muito mais fácil. Primeiramente, é preciso incluir a biblioteca no seu código:
```
#include <ArduinoJson.h>
```
Agora, podemos utilizar funções da biblioteca para criar, ler e editar dados em formato JSON. Por exemplo, podemos criar um objeto chamado "dados" que contém duas variáveis do tipo inteiro e uma string:
```
StaticJsonDocument<200> doc;
doc["idade"] = 25;
doc["numero_telefone"] = 123456789;
doc["sobrenome"] = "Silva";
```
Para visualizar esses dados em formato JSON, utilizamos a função printTo() e passamos um objeto do tipo Serial como parâmetro:
```
Serial.printTo(doc);
```
Isso irá imprimir os dados no monitor serial em formato JSON:
```
{
  "idade": 25,
  "numero_telefone": 123456789,
  "sobrenome": "Silva"
}
```

## Mergulho profundo:
JSON, que significa JavaScript Object Notation, foi criado em 2001 por Douglas Crockford como um formato de dados leve e legível por humanos. Alternativas para JSON incluem XML e CSV, mas JSON se tornou mais popular devido à sua simplicidade e flexibilidade. Para trabalhar com JSON em detalhes, é preciso entender seus tipos de dados (objetos, arrays, strings, inteiros, booleanos, etc.) e suas regras de formatação (uso de {} e [] para objetos e arrays, respectivamente). A biblioteca ArduinoJson utiliza um tipo de documento chamado StaticJsonDocument, específico para microcontroladores com pouca memória RAM disponível.

## Veja também:
- [Documentação oficial da biblioteca ArduinoJson](https://arduinojson.org/)
- [Tutorial em vídeo sobre como trabalhar com JSON no Arduino](https://www.youtube.com/watch?v=re1inm-jPCM)