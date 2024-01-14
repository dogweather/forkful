---
title:    "Kotlin: Utilizando expressões regulares"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Por que usar expressões regulares em Kotlin?

As expressões regulares são padrões de caracteres que permitem que você encontre e manipule texto de forma eficiente. Em Kotlin, elas são implementadas pela classe `Regex` e podem ser usadas para tarefas como validação de entrada de usuário, extração de dados de texto e substituição de padrões.

## Como usar expressões regulares em Kotlin

Usar expressões regulares em Kotlin é relativamente simples. Primeiramente, você precisa criar uma instância da classe `Regex` passando o padrão que deseja procurar como argumento. Em seguida, basta chamar o método `find()`, `match()`ou `replace()` com a string que deseja manipular.

Veja um exemplo de como validar um endereço de e-mail usando uma expressão regular em Kotlin:

```Kotlin
val emailRegex = Regex("[a-z0-9._%+-]+@[a-z0-9.-]+\\.[a-z]{2,}")
val email = "exemplo@exemplo.com"

if (emailRegex.find(email) != null) {
    println("Endereço de e-mail válido!")
} else {
    println("Endereço de e-mail inválido!")
}
```
Saída:
`Endereço de e-mail válido!`

## Mergulhando mais fundo nas expressões regulares em Kotlin

As expressões regulares em Kotlin podem ser combinadas com diversos métodos de string para uma manipulação mais complexa de texto. Por exemplo, você pode usar o método `find()` juntamente com o método `groupValues` para obter os valores correspondentes a cada grupo na expressão regular.

Além disso, a classe `Regex` possui diversos métodos, como `replaceFirst()` e `replace()` que permitem substituir padrões em uma string. Você também pode usar o método `toPattern()` para obter o objeto `Pattern` correspondente à expressão regular, caso deseje utilizar em outras linguagens de programação.

Para mais informações sobre o uso de expressões regulares em Kotlin, consulte a [documentação oficial](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/kotlin.text/-regex/).

# Veja também

- [Documentação oficial do Kotlin sobre expressões regulares](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/kotlin.text/-regex/)
- [Exemplo de uso de expressões regulares em Kotlin](https://www.baeldung.com/kotlin-regex)