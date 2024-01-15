---
title:                "Utilizando expressões regulares"
html_title:           "Arduino: Utilizando expressões regulares"
simple_title:         "Utilizando expressões regulares"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por que usar expressões regulares no Arduino?

As expressões regulares são uma ferramenta útil e poderosa para manipulação de padrões em cadeias de texto. Elas podem ser úteis para filtrar e validar dados, além de permitir uma maior flexibilidade na busca e substituição de informações. No Arduino, o uso de expressões regulares pode ser especialmente útil em projetos que envolvem comunicação com outros dispositivos ou em aplicações que envolvem a manipulação de dados.

## Como usar expressões regulares no Arduino?

Para utilizar expressões regulares no Arduino, é necessário incluir a biblioteca "Regex.h" no seu código. Em seguida, é preciso criar um objeto da classe "Regex" e utilizar os métodos disponíveis para realizar as operações desejadas.

Por exemplo, se quisermos verificar se uma determinada entrada de usuário é um número de telefone válido, podemos utilizar o seguinte código:

```Arduino
#include <regex.h>

Regex phoneRegex("[0-9]{2,3} [0-9]{4,5}-[0-9]{4}");

String userInput = "11 91234-5678";

boolean valid = phoneRegex.match(userInput);

if(valid){
  Serial.println("Número de telefone válido!");
}
else{
  Serial.println("Número de telefone inválido.");
}
```

No código acima, criamos um objeto "phoneRegex" que irá verificar se o padrão de um número de telefone está sendo seguido (2 a 3 dígitos + espaço + 4 a 5 dígitos + hífen + 4 dígitos). Em seguida, utilizamos o método "match" para verificar se a string de entrada do usuário é válida de acordo com esse padrão. Caso seja válido, uma mensagem será exibida no Serial Monitor.

## Deep Dive: Trabalhando com expressões regulares

Para utilizar expressões regulares de forma mais avançada no Arduino, é importante entender os diferentes caracteres e operadores utilizados nessa linguagem. Algumas das principais características das expressões regulares são:

- Os colchetes ([]) são utilizados para indicar um conjunto de caracteres possíveis. Por exemplo, [aeiou] irá buscar por todas as vogais em uma string.
- O asterisco (*) indica que o elemento anterior pode aparecer zero ou mais vezes. Por exemplo, o padrão "a*" irá buscar por todas as ocorrências da letra "a".
- O sinal de mais (+) indica que o elemento anterior pode aparecer uma ou mais vezes. Por exemplo, o padrão "b+" irá buscar por todas as sequências de "b".
- Os parênteses () são utilizados para agrupar elementos e criar subexpressões. Por exemplo, (ab)* irá buscar por qualquer número de ocorrências da sequência "ab".
- O ponto (.) indica ocorrência de qualquer caractere, exceto quebra de linha.
- O sinal de interrogação (?) indica que o elemento anterior pode aparecer zero ou uma vez.
- O caractere de escape (\) é utilizado para indicar que o próximo caractere deve ser interpretado literalmente, ao invés de realizar uma função especial.

Para aprofundar seus conhecimentos em expressões regulares e suas aplicações no Arduino, você pode conferir os links abaixo:

- [Tutorial de Expressões Regulares para Arduino](https://www.link.com)
- [Documentação da Biblioteca Regex.h](https://www.link.com)
- [Exemplos de uso de expressões regulares no Arduino](https://www.link.com)

## Veja também
- [Exemplos de uso de expressões regulares em projetos do Arduino](https://www.projeto1.com)
- [Biblioteca Regex.h no Arduino Reference](https://www.projeto2.com)