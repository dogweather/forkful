---
title:                "Encontrando o comprimento de uma string"
html_title:           "Arduino: Encontrando o comprimento de uma string"
simple_title:         "Encontrando o comprimento de uma string"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que encontrar o comprimento de uma string?

Normalmente, queremos descobrir o comprimento de uma string por razões de formatação, validação de entrada de dados ou simplesmente por curiosidade. Saber o comprimento de uma string pode ser útil em muitas situações de programação.

## Como fazer:

O Arduino possui uma função built-in chamada "strlen" que nos permite encontrar o comprimento de uma string. Vamos dar uma olhada em um exemplo:

```Arduino
String minhaString = "Olá, mundo!";
int comprimento = strlen(minhaString);
Serial.print("O comprimento da string é: ");
Serial.println(comprimento);
```
Output:
```
O comprimento da string é: 12
```
Neste exemplo, declaramos uma variável do tipo String e atribuímos a ela a frase "Olá, mundo!". Em seguida, usamos a função strlen() para encontrar o comprimento dessa string e atribuímos o valor retornado pela função à variável "comprimento". Por fim, imprimimos o valor na porta serial.

## Mergulho profundo:

A função strlen retorna um valor do tipo "size_t", ou seja, um valor inteiro não sinalizado (positivo) que representa o comprimento de uma string. Além disso, é importante ressaltar que os espaços em branco também são contabilizados no comprimento de uma string.

Outra forma de encontrar o comprimento de uma string no Arduino é usando o loop "for". Vamos dar uma olhada em como ficaria nosso exemplo utilizando essa técnica:

```Arduino
String minhaString = "Olá, mundo!";
int comprimento = 0;
for (int i = 0; i < minhaString.length(); i++) {
  comprimento++;
}
Serial.print("O comprimento da string é: ");
Serial.println(comprimento);
```
Output:
```
O comprimento da string é: 12
```
Neste exemplo, percorremos cada letra da string utilizando o método "length()", que nos retorna o comprimento total da string. A cada iteração do loop, incrementamos o valor da variável "comprimento". Ao final, o valor final é impresso na porta serial.

## Veja também:
- Documentação oficial da função strlen do Arduino: https://www.arduino.cc/reference/pt/language/variables/data-types/string/functions/strlen/
- Artigo do Alura sobre manipulação de strings no Arduino: https://www.alura.com.br/artigos/manipulando-strings-no-arduino-com-strings
- Vídeo tutorial do canal "Programação Descomplicada" sobre strings no Arduino: https://www.youtube.com/watch?v=9bXZE5bEeiI