---
title:                "Interpolando uma string"
html_title:           "Arduino: Interpolando uma string"
simple_title:         "Interpolando uma string"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/interpolating-a-string.md"
---

{{< edit_this_page >}}

## O que & Por quê?

Interpolar uma string significa combinar partes de outras strings e variáveis ​​para formar uma nova string. Os programadores fazem isso para economizar tempo e evitar repetição de código ao criar mensagens personalizadas ou formatar texto.

## Como fazer:

```
//Exemplo 1: Interpolação simples com dois valores variáveis
int idade = 25;
String mensagem = "Eu tenho " + idade + " anos."; //A string resultante será "Eu tenho 25 anos."

//Exemplo 2: Utilizando interpolação em uma condição if
int contador = 10;
if (contador > 5) {
  Serial.println("O contador é maior que 5: " + String(contador)); //A mensagem será "O contador é maior que 5: 10"
}

//Exemplo 3: Interpolação em uma função
float temperatura = 25.5;
void setup() {
  Serial.begin(9600);
  Serial.println("A temperatura é: " + String(temperatura, 1) + " graus Celsius."); //A mensagem será "A temperatura é: 25.5 graus Celsius."
}
void loop() {
  //faz alguma coisa
}
```

## Mergulho profundo:

Interpolação de string é uma técnica comum em programação devido à sua praticidade e eficiência. Antes do seu surgimento, os programadores precisavam utilizar métodos complexos para formatar strings com variáveis. Existem diferentes formas de se fazer interpolação, como o comando `printf()` ou a função `format()`, porém a interpolação de string se tornou a mais popular por sua simplicidade e facilidade de leitura.

## Veja também:

- [Documentação oficial do Arduino sobre Strings](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [Tutorial em vídeo sobre interpolação de string no Arduino](https://www.youtube.com/watch?v=x98AAYRKQN0)