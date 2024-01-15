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

Por que: Por que se envolver com JSON utilizando o Arduino pode ser muito útil para criar projetos que exigem a comunicação de dados com outros dispositivos ou plataformas, como a internet.

Como fazer: Para começar a trabalhar com JSON no Arduino, é necessário instalar uma biblioteca específica chamada "ArduinoJson". Ela pode ser facilmente encontrada e instalada através do Gerenciador de Bibliotecas no IDE do Arduino. Em seguida, basta importá-la para seu código utilizando o comando ```#include <ArduinoJson.h>```.
Com a biblioteca instalada, podemos criar um objeto JSON utilizando a seguinte sintaxe: 
```Arduino
StaticJsonDocument<500> doc; 
```
Aqui, definimos o tamanho máximo que nosso objeto pode ter (no exemplo, 500 bytes). Em seguida, podemos inserir dados nesse objeto utilizando o formato "chave: valor". Por exemplo:
```Arduino
doc["nome"] = "John";
doc["idade"] = 25;
```
Para enviar esse objeto JSON, utilizamos a função ```serializeJson()```, passando como parâmetros o objeto e o objeto responsável pelo envio de dados (exemplo: ```Serial``` para enviar pelos pinos serial do Arduino). E para receber e interpretar um objeto JSON, utilizamos a função ```deserializeJson()```, que converte os dados recebidos em formato JSON para um objeto manipulável no Arduino.
É importante lembrar que a sintaxe do JSON é sensível a espaços e caracteres especiais, então é preciso ter cuidado ao digitar os dados.

Deep Dive: Um objeto JSON é basicamente uma estrutura de dados que permite armazenar informações de forma organizada. Ele consiste em pares de chave e valor, onde a chave é uma string que identifica o valor relacionado a ela. O valor pode ser um número, uma string, um objeto ou um array. 
No Arduino, podemos trabalhar com objetos JSON de duas maneiras: estática e dinâmica. No exemplo acima, utilizamos a maneira estática, onde definimos o tamanho máximo do objeto de antemão. Porém, se não soubermos com exatidão o tamanho do objeto, podemos utilizar a maneira dinâmica, onde não precisamos definir o tamanho antes e o objeto vai se adaptando conforme os dados são inseridos. Isso pode ser feito utilizando a sintaxe:
```Arduino
DynamicJsonDocument doc(500); 
```
Neste caso, o tamanho máximo é passado como parâmetro para a criação do objeto.
Além disso, existem diversas funções na biblioteca ArduinoJson que nos permitem manipular e obter informações sobre o objeto JSON, como por exemplo, acessar e alterar valores específicos utilizando a sintaxe "objeto["chave"].valor" ou realizar loops para percorrer todos os valores.

Veja também: Para mais informações sobre a biblioteca ArduinoJson, recomendo a leitura da documentação oficial em https://arduinojson.org/ e a utilização de tutoriais e exemplos disponíveis na internet. Happy coding!