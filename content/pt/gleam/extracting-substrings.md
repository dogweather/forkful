---
title:    "Gleam: Extraindo subcadeias de caracteres"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que

Extrair substrings é uma habilidade importante para ter em sua caixa de ferramentas de programação. Ela permite que você manipule e trabalhe com partes específicas de uma string, o que pode ser útil em várias situações, como formatação de dados ou filtragem de informações.

## Como fazer

Para extrair substrings em Gleam, você pode usar o módulo `String` e sua função `slice`. Essa função aceita três argumentos: a string original, o índice inicial e o índice final da substring desejada. Aqui está um exemplo de como usar essa função:

```Gleam
import String

fn main() {
  let my_string = "Olá, mundo!"
  let substring = String.slice(my_string, 5, 10)
  // o valor de substring será "mundo"
}
```

Você também pode usar índices negativos para extrair substrings a partir do final da string. Por exemplo, `String.slice(my_string, -6, -1)` resultaria em "mundo". Além disso, a função `len` do módulo `String` pode ser usada para obter o tamanho de uma string, o que pode ser útil para determinar os índices corretos para a sua substring.

## Profundidade

Extrair substrings em Gleam é relativamente simples, mas há algumas coisas a se ter em mente. Primeiro, os índices são baseados em zero, o que significa que o primeiro caractere de uma string tem o índice 0. Além disso, o índice final não é incluído na substring, ou seja, `String.slice(my_string, 0, 2)` resultaria em apenas o primeiro e o segundo caractere da string original.

Também é importante mencionar que, embora possa parecer mais eficiente usar índices negativos para extrair substrings a partir do final da string, isso pode resultar em resultados inesperados se a string contiver caracteres Unicode complexos.

## Veja também

- Documentação oficial de `String.slice` em Gleam: https://gleam.run/core/string.html#slicestring-int-int
- Exemplos de uso de `String.slice` em Gleam: https://github.com/gleam-lang/gleam/blob/main/stdlib/core/tests/string.gleam#L37