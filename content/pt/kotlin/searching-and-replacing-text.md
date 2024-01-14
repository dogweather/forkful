---
title:                "Kotlin: Buscando e substituindo texto"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Por que engajar na busca e substituição de texto?

Frequentemente, durante a escrita de códigos em Kotlin, pode ser necessário realizar busca e substituição de texto. Isso pode ser necessário para corrigir erros, modificar variáveis ou fazer alterações em diferentes partes do código. Às vezes, pode ser um processo tedioso e demorado, mas existem maneiras de torná-lo mais eficiente e eficaz.

## Como fazer busca e substituição de texto em Kotlin

Para realizar a busca e substituição de texto de forma mais eficiente, é importante conhecer as diferentes ferramentas e métodos disponíveis em Kotlin. Uma forma de fazer isso é utilizando o recurso "Find and Replace" (Encontrar e Substituir) nativo do IDE (Ambiente de Desenvolvimento Integrado) que você está utilizando. Vamos ver alguns exemplos de como realizar a busca e substituição de texto utilizando o IntelliJ IDEA.

```Kotlin
// Exemplo 1: Utilizando a tecla de atalho "Ctrl + R" para abrir a janela de busca e substituição
val nome = "Maria"
val sobrenome = "Silva"

```

```Kotlin
// Exemplo 2: Utilizando a tecla de atalho "Ctrl + Shift + R" para abrir a janela de busca por palavra e substituição
val nome = "João"
val sobrenome = "Santos"

```

Ao utilizar a janela de busca e substituição, você pode procurar por uma string específica e substituí-la por outra em todo o seu código, ou apenas em uma parte selecionada dele. Isso pode ser feito facilmente com as opções de "Replace" (Substituir) e "Replace All" (Substituir Todos).

## Aprofundando-se na busca e substituição de texto em Kotlin

Além do recurso "Find and Replace", existem outras maneiras de realizar a busca e substituição de texto em Kotlin. Você também pode utilizar expressões regulares (regex) para tornar o processo mais dinâmico e abrangente.

```Kotlin
// Exemplo 3: Utilizando expressões regulares para substituir múltiplas ocorrências de um padrão em uma string
val frase = "Hoje é um dia ensolarado e bonito."
frase.replace(Regex("[e]"), "ou")
// Output: "Houjou é um dia onsolaradou e bonitou."

```

Ao usar expressões regulares, você pode procurar por padrões específicos em uma string e substituí-los por outro valor. Além disso, existem bibliotecas de terceiros, como o "Kotlin-String-Replace" que podem facilitar ainda mais esse processo de busca e substituição de texto em seu código.

# Veja também

- [Documentação oficial do Kotlin sobre "Find and Replace"](https://kotlinlang.org/docs/basic-editing.html#find-and-replace)
- [Tutorial do Kotlin-String-Replace](https://www.baeldung.com/kotlin/replace-all-string-characters)
- [Guia do JetBrains sobre utilização de expressões regulares em Kotlin](https://www.jetbrains.com/help/idea/using-regular-expressions.html#find)