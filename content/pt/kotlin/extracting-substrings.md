---
title:                "Extraindo subcadeias de caracteres"
html_title:           "Kotlin: Extraindo subcadeias de caracteres"
simple_title:         "Extraindo subcadeias de caracteres"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

O que e por que?

Se você é um programador em Kotlin, provavelmente já precisou extrair substrings de uma string em algum momento. Extrair substrings é basicamente pegar partes específicas de uma string maior. Os programadores geralmente fazem isso para manipular ou analisar dados de uma string de forma mais eficiente.

Como fazer:

Kotlin fornece métodos úteis para extrair substrings de uma string. Veja alguns exemplos abaixo:

```Kotlin
val minhaString = "Olá, Mundo!"
print(minhaString.substring(0, 4)) // Saída: Olá
print(minhaString.substring(5, 11)) // Saída: Mundo!
```

Você também pode usar o método ```subSequence()``` para extrair uma sequência de caracteres de uma string.

```Kotlin
val minhaString = "123456789"
print(minhaString.subSequence(2, 5)) // Saída: 345
print(minhaString.subSequence(6, 9)) // Saída: 789
```

Você também pode usar o operador colchetes ```[]``` para acessar substrings de uma string.

```Kotlin
val minhaString = "Kotlin é incrível!"
print(minhaString[0..5]) // Saída: Kotlin
print(minhaString[10..17]) // Saída: incrível
```

Mergulho Profundo:

Extrair substrings é uma técnica comum de programação usada há décadas. No passado, era necessário usar loops e condicionais para manipular strings, mas com o avanço da tecnologia, novos métodos e operadores foram introduzidos para facilitar a extração de substrings.

Há também várias maneiras de extrair substrings em Kotlin, como usar expressões regulares ou usar funções de bibliotecas externas. No entanto, os métodos nativos fornecidos pela linguagem são geralmente mais eficientes e suficientes para a maioria dos casos de uso.

Veja também:

- [Documentação oficial do Kotlin sobre substrings](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string/substring.html)
- [Exemplos de uso de substrings em Kotlin](https://www.baeldung.com/kotlin/substring)
- [Tópico da comunidade sobre extração de substrings em Kotlin](https://discuss.kotlinlang.org/t/is-there-a-txtloadfunction/4237/5)