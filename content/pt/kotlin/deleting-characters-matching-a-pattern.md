---
title:                "Excluindo caracteres que correspondem a um padrão"
html_title:           "Arduino: Excluindo caracteres que correspondem a um padrão"
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Remover caracteres que correspondem a um padrão é uma operação comum em programação onde procuramos e descartamos caracteres específicos em uma string. Programadores fazem isso por várias razões, como limpar de dados, manipulação de texto ou implementação de regras de negócios.

## Como Fazer:

Aqui está um exemplo de como fazer usando a função `replace` em Kotlin. Esta função substitui os caracteres que correspondem ao padrão por uma string vazia (""), em outras palavras, removendo-os.

```Kotlin
fun main() {
    val str = "Exemplo123"
    val novaStr = str.replace("[0-9]".toRegex(), "")
    println(novaStr)  // Impressão: "Exemplo"
}
```
Neste caso, o output será "Exemplo" porque removemos todos os dígitos que correspondem ao padrão "[0-9]".

## Mergulho Profundo

Historicamente, manipulação de texto já foi algo caro computacionalmente. No entanto, com linguagens modernas e máquinas eficientes, isso se tornou trivial. Em Kotlin, alternativas para a função de `replace` poderiam ser a implementação manual de uma função que itera sobre a string e constrói uma nova string ou o uso de bibliotecas de terceiros que fornecem funções para manipulação de string.

A `replace` em Kotlin usa internamente a classe `Matcher` da Java, isto é, aplica o `Pattern.matcher` ao input, em seguida, chama `Matcher.replaceAll`. Ganhamos eficiência em operações de substituição por causa da implementação de alto nível.

## Veja Também

- [Documentação oficial Kotlin para funções de String](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string/index.html)
- [Tutoriais de Regex no Regexone](https://regexone.com)
- [Referência da API Java para a classe Matcher](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/util/regex/Matcher.html)