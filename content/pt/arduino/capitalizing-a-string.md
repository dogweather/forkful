---
title:                "Capitalizando uma string"
html_title:           "Arduino: Capitalizando uma string"
simple_title:         "Capitalizando uma string"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Arduino: Capitalização de Strings

## O Que & Porquê?

Capitalizar uma string envolve transformar todas as letras minúsculas em maiúsculas. Programadores fazem isso para padronizar a aparência dos dados de texto ou para comparações de strings sensíveis a maiúsculas e minúsculas.

## Como Fazer:

Para capitalizar uma string no Arduino, usamos a função `toUpperCase()`. Aqui estão alguns exemplos:

```arduino
String minhaString = "aprendendo a programar";
minhaString.toUpperCase();
Serial.println(minhaString);  //imprime: APRENDENDO A PROGRAMAR
```

```arduino
char minhaString[] = "aprendendo a programar";
for(int i = 0; minhaString[i]; i++){
  minhaString[i] = toupper(minhaString[i]);
}
Serial.println(minhaString);  //imprime: APRENDENDO A PROGRAMAR
```

## Mergulho Profundo

Historicamente, a necessidade de capitalizar strings surgiu nos primórdios da ciência da computação, quando os sistemas eram estritamente sensíveis a maiúsculas e minúsculas. Ainda é relevante hoje, especialmente ao lidar com comparação de strings, nomes de usuário e senhas.

Existem várias alternativas para a função `toUpperCase()`. Se você quisesse apenas a primeira letra maiúscula, poderia usar `charAt(0)` com `toUpperCase()` em combinação com um slice do restante da string.

No nível de implementação, `toUpperCase()` percorre cada caractere da string e usa a tabela ASCII para transformar letras minúsculas em maiúsculas.

## Veja Também

- Guide completo de referência do Arduino String Library em [https://www.arduino.cc/reference/en/language/variables/data-types/string/](https://www.arduino.cc/reference/en/language/variables/data-types/string/)

- Dicas detalhadas sobre a manipulação de String no Arduino, disponíveis em [https://startingelectronics.org/software/arduino/learn-to-program-course/11-strings/](https://startingelectronics.org/software/arduino/learn-to-program-course/11-strings/)

- Informações adicionais sobre funções de string no Arduino, você pode encontrar em [https://www.robocore.net/tutoriais/funcoes-da-classe-string-no-arduino.html](https://www.robocore.net/tutoriais/funcoes-da-classe-string-no-arduino.html)