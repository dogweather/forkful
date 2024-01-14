---
title:    "Gleam: Capitalizando uma string."
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Por que

Ao trabalhar com strings em programação, muitas vezes nos deparamos com a necessidade de alterar a capitalização de uma palavra ou frase. Isso pode ser útil em situações como criar títulos para textos ou em validações de dados. Felizmente, a linguagem de programação Gleam possui uma função simples e eficiente para essa tarefa.

## Como fazer

A função `String.capitalize` é responsável por alterar a capitalização de uma string, de acordo com as regras da língua inglesa. Para utilizá-la, basta passar a string desejada como argumento e a função irá retornar a mesma string com a primeira letra em maiúscula.

```
Gleam def capitalized_string = String.capitalize("gleam programming")

// Saída: "Gleam programming"
```

Caso a string já esteja com a primeira letra maiúscula, a função irá retornar a mesma sem alterações. Veja um exemplo:

```
Gleam def capitalized_string = String.capitalize("Gleam programming")

// Saída: "Gleam programming"
```

Além disso, a função também pode ser utilizada para alterar a capitalização de apenas a primeira letra de cada palavra de uma frase. Para isso, basta utilizar a função `String.capitalize_words` e passar a frase como argumento.

```
Gleam def capitalized_phrase = String.capitalize_words("learn to code in gleam")

// Saída: "Learn To Code In Gleam"
```

## Profundidade

A função `String.capitalize` utiliza o padrão Unicode para determinar quais caracteres devem ser convertidos para maiúscula. Isso significa que não só letras do alfabeto, mas também números e caracteres especiais podem ser alterados pela função.

Caso seja necessário alterar a capitalização de uma string de acordo com as regras da língua portuguesa ou de outro idioma, é possível utilizar a função `String.to_title_case`, que aceita um argumento adicional indicando o idioma desejado.

```
Gleam def capitalized_string = String.to_title_case("aprenda a programar em gleam", "portuguese")

// Saída: "Aprenda a Programar em Gleam"
```

## Veja também

- Documentação oficial da função `String.capitalize` na [página do Gleam](https://gleam.run/modules/gleam_std.string.html#function.capitalize)
- Artigo sobre capitalização de strings em [odin.software](https://odin.software/articles/capitalize-string/)