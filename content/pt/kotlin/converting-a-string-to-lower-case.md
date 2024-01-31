---
title:                "Convertendo uma string para minúsculas"
date:                  2024-01-20T17:38:53.066699-07:00
model:                 gpt-4-1106-preview
simple_title:         "Convertendo uma string para minúsculas"

category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?
Converter strings para minúsculas significa transformar todas as letras de uma string para a sua forma minúscula. Programadores fazem isso para padronizar texto, facilitar comparações e atender a aspectos de usabilidade e acessibilidade.

## How to:
Em Kotlin, é bem simples converter uma string para minúsculas:

```Kotlin
fun main() {
    val originalString = "Olá, Kotlin!"
    val lowerCaseString = originalString.lowercase()

    println(lowerCaseString)  // Saída: "olá, kotlin!"
}
```

## Deep Dive
Converter strings não é uma invenção recente, vem desde os primórdios da informática onde a consistência dos dados sempre foi um ponto crucial. Em Kotlin, o método `lowercase()` substituiu o antigo `toLowerCase()` em versões recentes, aderindo a um estilo mais idiomático do Kotlin. Uma alternativa para considerar é o `toLowerCase(Locale)`, que faz a conversão respeitando regras de localização específicas — algo essencial para tratar caracteres fora do alfabeto inglês corretamente.

Internamente, a função `lowercase()` percorre cada caractere da string e converte para sua forma minúscula usando regras Unicode. É importante lembrar que a função não modifica a string original, e sim cria uma nova. Por isso, é considerada "pura", já que não tem efeitos colaterais.

## See Also
- Unicode case mappings: [Unicode Standard](https://www.unicode.org/reports/tr21/tr21-5.html)
