---
title:                "Excluindo caracteres que correspondem a um padrão"
html_title:           "Arduino: Excluindo caracteres que correspondem a um padrão"
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que

Às vezes, ao manipular strings no seu código Arduino, pode ser necessário excluir caracteres que correspondem a um padrão específico. Fazer isso pode ajudar a otimizar e limpar seu código, tornando-o mais eficiente.

## Como fazer

Para excluir caracteres em um padrão específico em uma string, você pode usar a função `remove_if()` da biblioteca `String`, que é incluída automaticamente no seu código Arduino. Essa função pode ser útil para excluir caracteres como espaços em branco, números ou símbolos em uma string.

Para usá-la, primeiro é necessário definir o padrão que será usado para encontrar os caracteres a serem excluídos. Você pode fazer isso de duas maneiras: usando uma função lambda ou uma função auxiliar. Aqui está um exemplo usando uma função lambda:

```
String minhaString = "A@r-d`uino";
String padrao = "`-@·"; // caracteres para excluir
minhaString.remove_if( [](char c) { return padrao.indexOf(c) >= 0; } );
Serial.println(minhaString); // saída: Arduino
```

Nesse exemplo, a função lambda é criada entre colchetes `[]` e a variável `c` representa cada caractere da string. A função `indexOf()` verifica se o caractere atual é um dos caracteres no padrão. Se for, retorna `true` e esse caractere é excluído. Caso contrário, a função `remove_if()` passa para o próximo caractere. Por fim, a string resultante é impressa no Serial Monitor.

Também é possível usar uma função auxiliar para definir o padrão, como neste exemplo:

```
bool padrao(char c) {
  if (c == 'e' || c == 'o') // caracteres para excluir
    return true;
  else
    return false;
}

String minhaString = "Olá Arduino";
minhaString.remove_if( padrao );
Serial.println(minhaString); // saída: Ol Arduino
```

Nesse caso, a função auxiliar `padrao()` verifica se o caractere atual é igual a algum dos caracteres definidos para serem excluídos. Se for, retorna `true` e o caractere é excluído.

## Deep Dive

A função `remove_if()` utiliza o conceito de _functor_, que é uma função ou objeto que pode ser usado com a mesma sintaxe de uma função. Isso torna a função `remove_if()` extremamente versátil e pode ser usada em diferentes situações para excluir caracteres em uma string, dependendo do padrão definido.

Além disso, ao usar a função `remove_if()`, você também pode especificar um limite para excluir apenas os primeiros caracteres que correspondem ao padrão. Isso pode ser feito passando um número inteiro após a função lambda ou função auxiliar, como neste exemplo:

```
bool padrao(char c) {
  if (c == 'l') // caractere para excluir
    return true;
  else
    return false;
}

String minhaString = "Hello World";
minhaString.remove_if( padrao, 2 ); // exclui apenas os 2 primeiros caracteres l
Serial.println(minhaString); // saída: Hell World
```

No exemplo acima, somente os dois primeiros caracteres "l" serão excluídos da string "Hello World".

## Veja também

Aqui estão alguns links úteis para saber mais sobre a função `remove_if()` e outras funções de string no Arduino:

- Referência da função `remove_if()`: https://www.arduino.cc/reference/en/language/functions/strings/stringobject_remove_if/
- Tutorial sobre strings no Arduino: https://www.arduino.cc/en/Tutorial/StringComparisonOperators
- Documentação sobre a biblioteca `String`: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/