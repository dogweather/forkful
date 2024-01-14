---
title:    "Kotlin: Unindo Strings"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que
Concatenar strings é uma técnica muito útil na programação Kotlin que permite juntar duas ou mais strings para criar uma única string. Isso é especialmente útil quando se lida com dados dinâmicos ou se deseja criar mensagens personalizadas.

## Como fazer
A concatenação de strings é muito fácil de fazer em Kotlin. Basta usar o operador "+" entre as strings que deseja unir. Por exemplo:

```Kotlin
val saudacao = "Olá"
val nome = "Maria"

val mensagem = saudacao + " " + nome + ", bem-vinda!"

println(mensagem)

//Saída: Olá Maria, bem-vinda!
```

Observe que também é possível incluir valores de variáveis ​​dentro da string concatenada, como demonstrado no exemplo acima.

Outra forma de concatenar strings é usando a função `plus()` da classe `String`. Isso permite que você adicione uma string ao final de outra string. Veja como:

```Kotlin
val primeiroNome = "João"
val sobrenome = "Silva"

val nomeCompleto = primeiroNome.plus(" ").plus(sobrenome)

println(nomeCompleto)

//Saída: João Silva
```

## Profundidade
Ao utilizar o operador "+" para concatenar strings em Kotlin, é importante lembrar que ele só funciona com valores do tipo `String`. Se você tentar usar o operador com valores de outros tipos, como inteiros ou booleanos, obterá um erro.

Também é possível concatenar mais de duas strings ao mesmo tempo. Basta repetir o operador "+" quantas vezes forem necessárias.

Uma dica importante é sempre ter cuidado ao concatenar strings que contenham caracteres especiais, como letras acentuadas ou símbolos. É recomendável utilizar a função `format()` ao invés do operador "+", pois ela garante que os caracteres serão tratados corretamente.

## Veja também
- [Documentação oficial do Kotlin sobre strings](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Artigo sobre formatação de strings em Kotlin](https://www.baeldung.com/kotlin-string-replace)
- [Vídeo tutorial sobre concatenação de strings em Kotlin](https://www.youtube.com/watch?v=gEVREZxQ-kc)