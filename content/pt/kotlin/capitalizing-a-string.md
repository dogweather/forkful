---
title:    "Kotlin: Maiúsculo de uma string"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Por que capitalizar uma string em Kotlin?

Capitalizar uma string em Kotlin pode ser útil em várias situações, como, por exemplo, padronizar a formatação dos dados exibidos para o usuário, ou para realizar comparações entre strings de forma mais precisa.

## Como fazer isso em Kotlin?

Para capitalizar uma string em Kotlin, podemos utilizar o método `capitalize()` da classe `String`. Por exemplo:

```Kotlin
val minhaString = "exemplo de string"
val stringCapitalizada = minhaString.capitalize()
println(stringCapitalizada)
```
Output:
```
Exemplo de string
```

Podemos também utilizar o método `toUpperCase()` da classe `String`, que irá transformar todos os caracteres da string em maiúsculos. Por exemplo:

```Kotlin
val minhaString = "exemplo de string"
val stringMaiuscula = minhaString.toUpperCase()
println(stringMaiuscula)
```
Output:
```
EXEMPLO DE STRING
```

## Mais informações sobre capitalização de strings

Ao capitalizar uma string em Kotlin, devemos estar atentos às diferenças entre maiúsculas e minúsculas. Por exemplo, o método `capitalize()` irá apenas capitalizar o primeiro caractere da string, enquanto o `toUpperCase()` irá transformar todos os caracteres em maiúsculos.

Além disso, ao trabalhar com strings em diferentes idiomas, é importante considerar que existem letras maiúsculas e minúsculas específicas para cada língua.

## Veja também

- [Documentação oficial do Kotlin sobre a classe String](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- [Tutorial sobre strings em Kotlin](https://www.baeldung.com/kotlin/strings)