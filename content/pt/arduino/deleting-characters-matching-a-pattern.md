---
title:                "Excluindo caracteres que correspondem a um padrão."
html_title:           "Arduino: Excluindo caracteres que correspondem a um padrão."
simple_title:         "Excluindo caracteres que correspondem a um padrão."
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## O que & Porquê?
Deletar caracteres que correspondem a um padrão é uma técnica usada por programadores para remover caracteres específicos de uma string. Isso pode ser útil para limpar dados ou para garantir que a string contenha apenas o que é necessário para ser processado.

## Como fazer:
```Arduino
String input = "Olá Mundo!";
input.replace("lá ", "");
Serial.println(input);
```
Saída:
```
O Mundo!
```
Neste exemplo, usamos o método `replace()` para substituir a string "lá " por uma string vazia, efetivamente removendo aquela parte da string original.

Outra opção é usar o método `remove()`, que permite especificar a posição inicial e o número de caracteres a serem removidos:
```Arduino
String input = "1234abcd";
input.remove(4, 4);
Serial.println(input);
```
Saída:
```
1234
```

## Mergulho Profundo:
A técnica de deletar caracteres que correspondem a um padrão é comumente usada em linguagens de programação e tem suas raízes no conceito de expressões regulares, que são padrões utilizados para identificar e manipular strings.

Além das opções mencionadas acima, também é possível deletar caracteres usando laços `for` e condicionais `if`, porém isso pode ser mais trabalhoso e menos eficiente do que usar métodos específicos da linguagem, como `replace()` e `remove()`.

Em alguns casos, pode ser necessário lidar com caracteres especiais, como espaços em branco ou símbolos, ao aplicar essa técnica, e é importante ter conhecimento sobre como esses caracteres são interpretados e tratados pelo programa.

## Veja também:
- [Documentação do método `replace()` em Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replace/)
- [Documentação do método `remove()` em Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/remove/)
- [Tutorial sobre expressões regulares em C++](https://www.cplusplus.com/reference/regex/regex/)