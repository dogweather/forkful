---
title:                "Arduino: Utilizando expressões regulares"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por que usar expressões regulares no Arduino?

Quando se trata de programar no Arduino, é importante ter ferramentas úteis para manipular strings e dados. As expressões regulares são uma dessas ferramentas que podem tornar sua programação mais eficiente e eficaz. Com elas, você pode pesquisar, validar e extrair informações específicas de uma string, economizando tempo e simplificando seu código.

## Como usar expressões regulares no Arduino?

Para utilizar expressões regulares no Arduino, é necessário importar a biblioteca <b>Regex</b>. Aqui está um exemplo de código que pesquisa um número de telefone em uma string:

```Arduino
// Importar a biblioteca 'Regex'
#include <Regex.h>

void setup() {
  Serial.begin(9600); // Inicializar a comunicação serial
  String texto = "Este é o meu número de telefone: 55 1234-5678"; // String fornecida para pesquisa
  
  // Criar um objeto de expressão regular para procurar por um número de telefone
  Regex meuNumero("(\\d{2}\\s\\d{4}-\\d{4})"); 

  // Se o número for encontrado, imprimi-lo no monitor serial
  if (meuNumero.find(texto)) {
    Serial.println("Número encontrado: " + meuNumero.match()); 
  } else {
    Serial.println("Número não encontrado."); // Caso contrário, imprima uma mensagem de erro
  }
}

void loop() {
  // Nada acontece no loop
}
```

A saída deste código seria: "Número encontrado: 55 1234-5678". Com a biblioteca Regex, você pode criar padrões de pesquisa mais complexos para extrair informações específicas de uma string.

## Aprofundando nas expressões regulares

As expressões regulares são uma linguagem poderosa para manipulação de strings. Elas são compostas por metacaracteres, que representam caracteres ou conjuntos de caracteres, e podem ser combinadas para criar padrões de pesquisa complexos. Alguns dos metacaracteres mais comuns são:

- <b>\d</b>: Qualquer dígito numérico de 0 a 9
- <b>\s</b>: Qualquer espaço em branco, incluindo tabulação e quebra de linha
- <b>[ ]</b>: Qualquer caractere dentro dos colchetes
- <b>{ }</b>: As chaves indicam quantos caracteres devem ser encontrados.

Por exemplo, o padrão "\d{2}\s\d{4}-\d{4}" utilizado no exemplo acima significa que a string deve começar com 2 dígitos seguidos de um espaço, seguido de 4 dígitos, um hífen e mais 4 dígitos. Isso corresponderá a qualquer número de telefone no formato "xx xxxx-xxxx".

Existem muitas outras combinações possíveis de metacaracteres e você pode usar a criatividade para criar padrões de pesquisa que atendam às suas necessidades. No entanto, é importante lembrar que, quanto mais complexo o padrão, mais tempo o Arduino levará para avaliá-lo, o que pode afetar o desempenho de seu código.

## Veja também

- [Documentação oficial da biblioteca Regex](https://www.arduino.cc/reference/en/libraries/regex/)
- [Tutorial sobre expressões regulares no Arduino](http://www.circuitbasics.com/basics-of-regex-with-arduino/)
- [Referência completa de metacaracteres](https://www.regular-expressions.info/quickstart.html)