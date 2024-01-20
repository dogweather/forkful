---
title:                "Pesquisando e substituindo texto"
html_title:           "Bash: Pesquisando e substituindo texto"
simple_title:         "Pesquisando e substituindo texto"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Procurar e substituir texto é uma operação comum que permite localizar uma string específica em um texto e substituí-la por outra. Os programadores usam essa função para corrigir erros, mudar variáveis ou adaptar códigos para novas implementações.

## Como fazer:

Vamos usar o método `String::replace()` da biblioteca string do Arduino. Esse método analisa uma string, procura uma sub-string especificada e a substitui por outra.

```
String text = "Ola, mundo!";
text.replace("mundo", "Arduino");
Serial.println(text);
```

Esse código substituirá "mundo" por "Arduino", portanto, o resultado impresso no Serial Monitor será "Ola, Arduino!".

## Mergulho Profundo

Esse método de busca e substituição é um truque útil e tem sua origem na programação adotada desde os primeiros compiladores. Existem alternativas, como o uso de expressões regulares para correspondência de padrões mais complexos, mas, por simplicidade e eficiência, o método de substituição de string é mais amplamente utilizado em pequenas aplicações, como aquelas destinadas ao Arduino.

### Implementação

Quando o `String::replace()` é chamado, ele percorre a string original da esquerda para a direita. Quando localiza a primeira ocorrência da sub-string de pesquisa, ele a substitui pela sub-string de substituição.

Ao contrário de alguns outros sistemas de programação, o Arduino não possui funções nativas para expressões regulares. Portanto, se precisar de uma funcionalidade de pesquisa e substituição mais avançada, talvez precise implementar uma biblioteca adicional.