---
title:    "Kotlin: Convertendo uma string para minúsculas"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Por que

Converter uma string para letras minúsculas é uma tarefa comum em muitos projetos de desenvolvimento de software. Essa conversão é útil para garantir a consistência nos dados, facilitar a comparação entre strings e melhorar a legibilidade do código.

## Como fazer

Para realizar a conversão de uma string para letras minúsculas em Kotlin, podemos usar o método `toLowerCase()` da classe `String`. Veja um exemplo abaixo:

```Kotlin
val string = "Olá Mundo"
val stringEmMinusculas = string.toLowerCase()
println(stringEmMinusculas) // saída: olá mundo
```

Podemos também usar o operador de extensão `?.let` para garantir que a conversão seja feita apenas se a string não for nula. Veja o exemplo abaixo:

```Kotlin
val string: String? = null
string?.let {
    val stringEmMinusculas = it.toLowerCase()
    println(stringEmMinusculas) // saída: null
}
```

## Mergulho profundo

Ao converter uma string para letras minúsculas em Kotlin, é importante entender que esse processo não modifica a string original, mas sim retorna uma nova string em caixa baixa. Além disso, devemos estar atentos aos caracteres acentuados, pois sua conversão para letras minúsculas pode variar dependendo do idioma e do sistema operacional utilizado.

Caso seja necessário realizar a conversão de caracteres acentuados, podemos utilizar a função `fold()` da classe `StringBuilder` em conjunto com a função `removeAccents()` da biblioteca `ICU4J`. Veja o exemplo abaixo:

```Kotlin
val string = "Árvore à Beira do Mar"
val stringEmMinusculas = StringBuilder(string.length).let {builder ->
    builder.append(string)
    builder.fold(“”,{acc,char ->
        acc.append(ICU.removeAccents(char))
    })
}
println(stringEmMinusculas) // saída: arvore a beira do mar
```

## Veja também

- Documentação oficial do método `toLowerCase()` (https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/to-lower-case.html)
- Documentação oficial do operador de extensão `?.let` (https://kotlinlang.org/docs/reference/scope-functions.html#-let)
- Documentação oficial da função `fold()` da classe `StringBuilder` (https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/fold.html)
- Biblioteca ICU4J (http://site.icu-project.org/home)