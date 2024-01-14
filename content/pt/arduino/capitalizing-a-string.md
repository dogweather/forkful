---
title:    "Arduino: Maiúsculas em uma string"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Por que capitalizar uma string em Arduino?

Então você está programando em Arduino e se depara com a necessidade de capitalizar uma string. Você pode estar se perguntando: por que eu deveria fazer isso? Bem, existem várias razões pelas quais alguém pode querer capitalizar uma string em Arduino. Por exemplo, pode ser necessário para exibir corretamente um texto em um display LCD, ou para comparar strings em um código de controle de acesso. Independentemente do motivo, capitalizar uma string pode ser uma habilidade útil a se ter ao programar em Arduino.

## Como capitalizar uma string em Arduino

Agora que entendemos por que capitalizar uma string pode ser útil, vamos aprender como fazer isso em Arduino. O processo é bastante simples e pode ser feito em apenas algumas etapas.

Primeiro, declare uma variável com o valor da string que você deseja capitalizar. Por exemplo, ```String minhaString = "ola mundo"```.

Em seguida, use a função ```toUpperCase()``` para converter a string em letras maiúsculas. Por exemplo, ```minhaString.toUpperCase()```.

Por fim, você pode imprimir a string capitalizada usando a função ```Serial.println()```, como por exemplo ```Serial.println(minhaString)```.

Seu código completo deve se parecer com isto:

```Arduino
String minhaString = "ola mundo";
minhaString.toUpperCase();
Serial.println(minhaString);
```

E o resultado da serial deve ser:

```
OLA MUNDO
```

## Mais informações sobre capitalizar uma string no Arduino

Agora que você sabe como capitalizar uma string em Arduino, vamos mergulhar em alguns detalhes adicionais sobre o assunto.

Uma coisa importante a ter em mente é que a função ```toUpperCase()``` só funciona com caracteres ASCII, o que significa que ela não irá capitalizar caracteres acentuados, como á, é, í, ó, ú. Se você precisar capitalizar uma string com caracteres acentuados, você pode criar uma função personalizada que substitua esses caracteres por suas versões maiúsculas.

Além disso, vale mencionar que a função ```toUpperCase()``` é uma função mutadora, o que significa que ela altera diretamente o valor da variável em que é aplicada. Isso pode ser importante se você estiver trabalhando com uma variável de string que precisa ser mantida em seu formato original.

## Veja também

Aqui estão algumas fontes adicionais onde você pode aprender mais sobre capitalizar strings em Arduino:

- [Documentação Oficial do Arduino para a Função toUpperCase()](https://www.arduino.cc/reference/pt/language/variables/data-types/string/functions/touppercase/)
- [Tutorial de Capitalização de Strings em Arduino](https://www.youtube.com/watch?v=weEWeMvls2o)
- [Fórum de Discussão sobre Capitalização de Strings no Arduino](https://forum.arduino.cc/t/capital-string-letter/109030)

Espero que este artigo tenha sido útil para você aprender a capitalizar strings em Arduino. Agora que você tem essa habilidade em seu arsenal de programação, você pode aplicá-la em seus projetos futuros. Divirta-se programando!