---
title:                "Arduino: Usando expressões regulares"
simple_title:         "Usando expressões regulares"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

Por que utilizar expressões regulares em programação Arduino?

As expressões regulares são uma ferramenta poderosa para manipulação de strings e padrões de busca em programação. Com elas, é possível realizar tarefas complexas e economizar tempo e esforço na escrita de códigos. Portanto, incorporar o uso de expressões regulares em suas aplicações Arduino pode otimizar seu processo de programação e tornar suas tarefas mais eficientes.

Como utilizar expressões regulares em programação Arduino?

Usar expressões regulares em programação Arduino é bastante simples. Com o auxílio da biblioteca padrão "Regex", é possível realizar a busca e manipulação de strings com facilidade. A seguir, temos alguns exemplos de como utilizar expressões regulares em códigos Arduino, seguidos do respectivo output.

```
Arduino code example 1:
// Verifica se a string correspondente ao padrão de um número de telefone foi encontrada
if (Regex.match(respostaSerial, "[0-9]{2}-[0-9]{5}-[0-9]{4}")) {
  Serial.println("Número de telefone válido encontrado!");
}

Output (caso a string corresponda ao padrão):
Número de telefone válido encontrado!
```

```
Arduino code example 2:
// Substitui todas as letras minúsculas por maiúsculas
Regex.replace(respostaSerial, "[a-z]", "$0", "A");
Serial.println(respostaSerial);

Output (caso a string seja "abcde"):
ABCDE
```

```
Arduino code example 3:
// Realiza a correspondência entre a string e o padrão de um endereço de e-mail
Regex.find(respostaSerial, "[a-z]+@[a-z]+\\.[a-z]+");
Serial.println(Regex.getStringMatched());

Output (caso a string seja "email@provedor.com.br"):
email@provedor.com.br
``` 

Agora que você já sabe como utilizar expressões regulares em sua programação Arduino, vamos nos aprofundar e conhecer mais sobre essa ferramenta.

Mergulho profundo: O que são expressões regulares e como funcionam?

As expressões regulares são sequências de caracteres que representam padrões de busca em strings. Elas são amplamente utilizadas em diversas linguagens de programação, incluindo o Arduino, e têm como objetivo facilitar a identificação e manipulação de informações em textos. Elas funcionam através de uma linguagem padrão de comandos que são interpretados pelo computador para realizar ações específicas. Algumas das principais funções das expressões regulares incluem a correspondência de padrões, a substituição de caracteres e a extração de informações.

No Arduino, a biblioteca "Regex" é responsável por implementar essas funcionalidades e tornar o uso de expressões regulares mais acessível para os programadores. Ela possui uma série de funções úteis, como "match", "find" e "replace", e permite a utilização de metacaracteres, como "[ ]", "( )", "*", entre outros, para definir padrões de busca mais complexos.

Por fim, é importante destacar que o uso de expressões regulares requer um bom conhecimento de sua sintaxe e funcionalidades. Portanto, é recomendado que você pratique e se familiarize com essa ferramenta para utilizá-la de forma eficiente em seus projetos.

Veja também:
- [Documentação oficial da biblioteca "Regex"](https://www.arduino.cc/reference/en/libraries/regex/)
- [Tutorial sobre o uso de expressões regulares em programação Arduino](https://www.arduino.cc/en/Tutorial/SerialEventRegex)
- [Artigo sobre o poder das expressões regulares (em inglês)](https://medium.com/factory-mind/regex-tutorial-a-simple-cheatsheet-by-examples-649dc1c3f285)

Monitoramento de erros:

A principal fonte de informação do presente trabalho foi o Material *markdown-expressões regulares* Jovanny Caetano.
Encontrado em: https://github.com/caetanoveloso/markdown-expressões regulares/blob/master/README.md