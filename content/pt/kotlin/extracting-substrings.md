---
title:    "Kotlin: Extraindo subcadeias"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Por que extrair substrings em Kotlin?

Ao trabalhar com strings em Kotlin, muitas vezes é necessário extrair uma parte específica de uma string maior para obter informações úteis. Isso pode ser feito através da extração de substrings. Este processo pode ser especialmente útil na manipulação de dados ou na implementação de algoritmos de busca e filtragem.

## Como extrair substrings em Kotlin

Extrair substrings em Kotlin é bastante simples, basta seguir o seguinte formato:

```Kotlin
val string = "Exemplo de string"
val substring = string.substring(beginIndex, endIndex)
```
Onde o `beginIndex` é o índice inicial da substring que você deseja extrair e o `endIndex` é o índice final (não incluso) da substring.

Por exemplo, se quisermos extrair a palavra "Exemplo" da string acima, usaríamos o seguinte código:

```Kotlin
val string = "Exemplo de string"
val substring = string.substring(0, 7)
println(substring)
```

Isso resultará na saída: `Exemplo`

Existem também outras formas de extrair substrings em Kotlin, como utilizando `subSequence()` ou `take()`.

### Função `subSequence()`

Esta função também permite a extração de substrings, porém utiliza um formato um pouco diferente:

```Kotlin
val string = "Exemplo de string"
val substring = string.subSequence(beginIndex, endIndex)
```

Neste caso, o `beginIndex` e o `endIndex` estão inclusos no resultado final, diferente da função `substring()`. Ou seja, para extrair a mesma palavra "Exemplo", utilizaríamos o seguinte código:

```Kotlin
val string = "Exemplo de string"
val substring = string.subSequence(0, 7)
println(substring)
```

Isso resultará na saída: `Exemplo ` (com um espaço após a palavra).

### Função `take()`

Outra forma de extrair substrings em Kotlin é utilizando a função `take()`, que funciona da seguinte forma:

```Kotlin
val string = "Exemplo de string"
val substring = string.take(num)
```

Onde `num` é o número de caracteres que serão extraídos da string, a partir do início da mesma. No nosso exemplo, extrairíamos a palavra "Exemplo" com o seguinte código:

```Kotlin
val string = "Exemplo de string"
val substring = string.take(7)
println(substring)
```

Isso resultará na saída: `Exemplo`

## Uma olhada mais profunda na extração de substrings

Agora que sabemos como extrair substrings em Kotlin, podemos nos aprofundar um pouco mais no assunto. A extração de substrings é feita através da manipulação dos índices da string original. O `beginIndex` representa o índice do primeiro caractere da substring, enquanto o `endIndex` representa o índice do último caractere (não incluso). Isso significa que ao utilizar a função `substring()`, se o `endIndex` for maior que o `beginIndex`, a substring será extraída do `beginIndex` até o `endIndex` - 1.

Além disso, as funções `substring()` e `subSequence()` também podem receber apenas um parâmetro, que representa o `beginIndex`, e neste caso a substring será extraída a partir do `beginIndex` até o final da string.

## Veja também

- [Documentação oficial de Kotlin - Extração de Substrings](https://kotlinlang.org/docs/reference/basic-types.html#substrings)
- [Exemplo prático de extração de substrings em Kotlin](https://devtut.com/tutoriais/extrair-substring-em-kotlin.m945)