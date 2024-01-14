---
title:                "Kotlin: Capitalizando uma string"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que Capitalizar uma String?

Capitalizar uma string é um processo simples, mas útil, para alterar o formato de uma palavra ou frase. Ele pode ser usado para tornar o texto mais legível ou para atender a requisitos específicos de formatação em uma aplicação.

## Como Fazer

Para capitalizar uma string em Kotlin, você pode usar o método `capitalize()` ou `toUpperCase()`, dependendo do resultado desejado. Veja exemplos abaixo:

```Kotlin
val frase1 = "esta é uma frase"
val frase2 = "esse é outro exemplo de frase"

println(frase1.capitalize())
// Saída: Esta é uma frase

println(frase2.toUpperCase())
// Saída: ESSE É OUTRO EXEMPLO DE FRASE
```

Ao usar o método `capitalize()`, a primeira letra de cada palavra na string será transformada em maiúscula. Já o método `toUpperCase()` transformará todas as letras em maiúsculas.

Você também pode capitalizar apenas a primeira letra da string usando o método `capitalizeFirst()`:

```Kotlin
val nome = "joão"

println(nome.capitalizeFirst())
// Saída: João
```

Além disso, é possível capitalizar apenas a primeira letra de uma palavra específica em uma string usando o método `replaceFirst()` em conjunto com o método `capitalize()`:

```Kotlin
val frase = "este exemplo é incrível"

println(frase.replaceFirst("exemplo", "Exemplo").capitalize())
// Saída: Este Exemplo é incrível
```

## Profundando

Ao capitalizar uma string em Kotlin, é importante ter em mente que o padrão de capitalização depende do idioma definido para a aplicação. Por exemplo, em um texto em português, a letra "ç" deve ser maiúscula após capitalização, enquanto em um texto em inglês ela deve se manter minúscula.

Além disso, ao usar o método `capitalize()`, é válido notar que apenas a primeira letra de cada palavra será alterada. Portanto, se a string contém uma palavra já toda em maiúscula, essa palavra permanecerá igual após a capitalização.

## Veja Também

- Documentação oficial do método `capitalize()` em Kotlin: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/capitalize.html
- Tutorial sobre capitalização de strings em Kotlin: https://www.baeldung.com/kotlin/capitalize-string
- Discussão sobre as diferenças entre os métodos `capitalize()` e `toUpperCase()` em Kotlin: https://stackoverflow.com/questions/38713697/what-is-difference-between-upper-robotin-android