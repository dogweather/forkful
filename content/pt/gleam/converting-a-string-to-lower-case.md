---
title:    "Gleam: Convertendo uma string para letras minúsculas"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

# Por que converter uma string para letras minúsculas em Gleam?

Às vezes, pode ser necessário converter uma string para letras minúsculas em um programa Gleam. Isso pode ser útil para comparar strings de forma mais precisa ou para formatar a saída exatamente como desejado. Felizmente, o processo é simples e flexível em Gleam.

# Como fazer isso em Gleam

Para converter uma string para letras minúsculas em Gleam, podemos usar a função `String.to_lower()`.

```Gleam
fn main() {
  let string = "Olá Mundo!"
  let result = String.to_lower(string)
  // Output: olá mundo!
}
```

Também podemos usar essa função diretamente em uma string literal:

```Gleam
fn main() {
  let result = "Olá Mundo!".to_lower()
  // Output: olá mundo!
}
```

Como você pode ver, a saída da função `to_lower()` é uma nova string convertida para todas as letras minúsculas.

# Mergulho profundo

A função `to_lower()` em Gleam usa o Unicode Case Folding para converter as letras maiúsculas para minúsculas. Isso significa que, além das letras do alfabeto latino, também são convertidas letras maiúsculas de outros idiomas, como o cirílico e o grego. Além disso, os caracteres especiais também são mantidos em suas formas minúsculas correspondentes.

Uma coisa importante a notar é que essa função é sensível a localização, então a saída pode variar dependendo do idioma configurado em seu sistema operacional.

# Veja também

- Documentação oficial do Gleam: https://gleam.run/
- Unicode Case Folding: https://unicode.org/standard/standard.html
- Tutorial de strings em Gleam: https://gleam.run/book/tour/strings.html