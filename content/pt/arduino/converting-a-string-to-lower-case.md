---
title:    "Arduino: Convertendo uma string para minúsculas"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Por que
Muitas vezes, em projetos de programação com Arduino, nos deparamos com a necessidade de converter strings para letras minúsculas. Isso pode ser útil, por exemplo, quando estamos trabalhando com sensores e precisamos comparar strings em nosso código.

## Como fazer
Para converter uma string para letras minúsculas no Arduino, podemos usar a função `toLowerCase()` da classe `String`. Veja um exemplo:

```Arduino
String frase = "Olá MUNDO!";
String fraseMinuscula = frase.toLowerCase();
Serial.println(fraseMinuscula); // saída: olá mundo!
```

Legal, não é? Agora podemos facilmente comparar a string convertida com outra string minúscula, sem nos preocupar com as letras maiúsculas.

## Mergulho Profundo
Internamente, a função `toLowerCase()` usa a tabela ASCII para converter as letras maiúsculas para minúsculas. Isso significa que apenas letras serão afetadas, sem alterar outros caracteres como números e símbolos.

Uma coisa importante a se observar é que a função `toLowerCase()` modifica a string original. Ou seja, a variável `frase` em nosso exemplo, também será convertida para letras minúsculas. Se não quisermos que isso aconteça, podemos criar uma cópia da string original e aplicar a função `toLowerCase()` a ela.

## Veja também
- [Referência da função `toLowerCase()`](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/tolowercase/)
- [Tabela ASCII](https://www.ascii-code.com/)

Obrigado por ler este artigo! Espero que tenha sido útil para você. Se tiver alguma dúvida ou sugestão, deixe um comentário abaixo. Até mais!