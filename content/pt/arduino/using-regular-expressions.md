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

# O que & Por quê?

Regular expressions (expressões regulares) são uma ferramenta útil para programadores que precisam fazer buscas e substituições específicas em texto. Elas permitem que você defina padrões de caracteres a serem encontrados, facilitando tarefas repetitivas de manipulação de strings em seus códigos.

# Como fazer:

As expressões regulares são amplamente utilizadas em linguagens de programação, e para incluí-las em seu código Arduino, é necessário utilizar a biblioteca padrão "Regex.h". Aqui está um exemplo simples de como utilizar expressões regulares para verificar se um número está presente em uma string:

```
Arduino Code Block:
// Incluindo a biblioteca Regex
#include <Regex.h>

// Define a expressão regular para encontrar o padrão de um número
Regex numberPattern("\d+");

// Define a string de teste
String str = "Este é um exemplo de string com o número 1234 presente.";

// Utiliza a função 'match' para verificar se a expressão regular é encontrada na string de teste
if (numberPattern.match(str)) {
  Serial.println("Número encontrado!");
}
else {
  Serial.println("Número não encontrado.");
}

```

Este código irá imprimir "Número encontrado!" na tela serial, pois a expressão regular encontrou um padrão numérico na string de teste.

# Mergulho Profundo:

As expressões regulares remontam aos anos 1950, quando foram utilizadas pela primeira vez em linguagens de programação específicas para manipulação de texto. Atualmente, elas são amplamente suportadas em várias linguagens de programação, incluindo C++, Java e Python.

No Arduino, a biblioteca "Regex.h" fornece funções úteis, como 'match' e 'search' para encontrar padrões em suas strings. Se você não estiver familiarizado com expressões regulares, pode parecer inicialmente difícil de entender, mas com estudo e prática, elas podem ser uma ferramenta poderosa para economizar tempo e esforço em suas tarefas de programação.

# Veja também:

- [Documentação oficial da biblioteca Regex.h](https://www.arduino.cc/reference/en/libraries/regex/)
- [Aprenda mais sobre expressões regulares com o livro "Mastering Regular Expressions"](https://www.oreilly.com/library/view/mastering-regular-expressions/0596528124/)
- [Tutorial sobre como utilizar expressões regulares no Arduino](https://www.hackster.io/jinithjm/using-regular-expression-regex-in-arduino-f84621)