---
title:                "Kotlin: Excluindo caracteres que correspondem a um padrão."
simple_title:         "Excluindo caracteres que correspondem a um padrão."
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que
Existem vários motivos pelos quais alguém pode precisar deletar caracteres que correspondem a um determinado padrão em um programa Kotlin. Pode ser necessário para limpar dados, formatar strings ou até mesmo otimizar o desempenho do código.

## Como Fazer
Para deletar caracteres que correspondem a um padrão em Kotlin, podemos usar a função `replace()` e expressões regulares. Vamos ver um exemplo simples:

```
Kotlin val string = "Olá Mundo!" val novaString = string.replace(Regex("[aeiou]"), "")
println(novaString) // Output: Ol Mnd!
```

Neste exemplo, usamos a função `replace()` para substituir quaisquer vogais na string `"Olá Mundo!"` por uma string vazia, resultando em `"Ol Mnd!"`. A expressão regular `[aeiou]` significa que queremos substituir qualquer letra que seja uma vogal.

Podemos usar expressões regulares mais complexas para atender às nossas necessidades, como deletar caracteres especiais, números ou até mesmo padrões específicos.

## Mergulho Profundo
Para entender melhor como deletar caracteres que correspondem a um padrão em Kotlin, é importante entender como funcionam expressões regulares. Elas são padrões utilizados para buscar e substituir strings em um texto.

No exemplo anterior, usamos `Regex("[aeiou]")` como argumento para a função `replace()`. Isso significa que estamos criando uma expressão regular que busca por qualquer caractere que seja uma vogal. Podemos usar caracteres especiais, quantificadores e grupos para criar expressões regulares mais complexas.

Além disso, é importante conhecer outras funções da classe `Regex` em Kotlin, como `find()`, `matchEntire()` e `replaceFirst()`, que também podem ser úteis quando se trata de deletar caracteres correspondentes a um padrão.

## Veja Também
- Documentação oficial do Kotlin sobre expressões regulares: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/
- Tutorial sobre o uso de expressões regulares em Kotlin: https://www.baeldung.com/kotlin/regex
- Artigo sobre otimização de desempenho usando expressões regulares em Kotlin: https://proandroiddev.com/optimizing-performance-using-regular-expressions-in-kotlin-8232bfcf6e7a