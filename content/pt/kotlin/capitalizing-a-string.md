---
title:                "Capitalizando uma string"
html_title:           "Kotlin: Capitalizando uma string"
simple_title:         "Capitalizando uma string"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Ao capitalizar uma string, transformamos a primeira letra de cada palavra para maiúscula. Os programadores costumam fazer isso para melhorar a legibilidade e apresentação dos dados, como por exemplo, ao exibir nomes próprios.

## Como Fazer:
Em Kotlin, podemos capitalizar strings usando a função `capitalize`. Veja o exemplo abaixo:
```Kotlin
fun main() {
    val frase = "olá, mundo!"
    println(frase.capitalize())
}
```
Saída:
```
Olá, mundo!
```

Se quisermos todas as palavras com a primeira letra maiúscula, podemos dividir a string em palavras e capitalizar cada uma delas:

```Kotlin
fun main() {
    val frase = "olá, mundo maravilhoso!"
    
    val fraseCapitalizada = frase.split(" ").joinToString(" ") { it.capitalize() }
    
    println(fraseCapitalizada)
}
```
Saída:
```
Olá, Mundo Maravilhoso!
```

## Aprofundamento
Embora a capitalização de strings pareça uma tarefa simples, sua implementação varia de acordo com o idioma e as regras de formatação. Historicamente, a capitalização se originou dos manuscritos medievais para destacar certas palavras e evoluiu para uso comum em textos e programação. 

Em Kotlin, além da função `capitalize`, temos a função `replaceFirstChar` que pode ser usada para o mesmo propósito mas oferece mais controle, permitindo-nos definir como queremos alterar o primeiro caractere.

Também vale considerar a capitalização em contextos multilíngues - a capitalização pode não funcionar da mesma maneira em diferentes idiomas. A localização adequada pode exigir o uso de bibliotecas e métodos específicos que respeitam as regras gramaticais dos idiomas em questão.

## Veja Também
[Aqui](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/capitalize.html) está a documentação oficial para a função `capitalize` no Kotlin.

Para uma lista mais completa de funções de strings em Kotlin, consulte [esta página](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string/index.html).

Para discutir tópicos mais avançados, como a capitalização correta de strings em vários idiomas, você pode explorar [esta discussão](https://stackoverflow.com/questions/5054995/how-to-capitalize-the-first-character-of-each-word-in-a-string) no StackOverflow.