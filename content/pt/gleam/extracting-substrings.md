---
title:    "Gleam: Extraindo subcadeias"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

# Por que extrair substrings em Gleam

Extrair substrings é uma tarefa útil e comum que pode ser realizada na linguagem de programação Gleam. Ao extrair substrings, você pode manipular e obter partes específicas de uma string de texto. Isso pode ser útil em várias situações, como manipular dados de entrada ou formatar textos para exibição.

# Como extrair substrings em Gleam

Para extrair substrings em Gleam, você pode usar a função `slice()` e especificar o índice inicial e o índice final da substring que deseja obter. Por exemplo, se você tiver a string "Gleam programming" e quiser obter apenas "programming", pode usar o seguinte código:

```Gleam
let string = "Gleam programming"
let substring = string.slice(6, string.len())
```

Ao executar esse código, a saída será a seguinte:

```
programming
```

Você também pode especificar um terceiro argumento opcional na função `slice()`, que corresponde ao passo de iteração. Por exemplo, se você quiser obter apenas as letras pares da mesma string, pode usar o seguinte código:

```Gleam
let string = "Gleam programming"
let substring = string.slice(1, string.len(), 2)
```

A saída será a seguinte:

```
eamprogrmig
```

# Detalhes sobre extrair substrings

Além da função `slice()`, Gleam também oferece outras opções para extrair substrings. A função `slice_before()` permite extrair uma substring até um determinado caractere, enquanto a função `slice_after()` permite extrair uma substring após um determinado caractere. Além disso, a função `split()` permite obter substrings separando uma string em partes, com base em um determinado caractere ou padrão.

# Veja também

- Documentação oficial para a função `slice()`: https://gleam.run/modules/gleam_lang/strings.html#slice
- Outras funções de manipulação de strings em Gleam: https://gleam.run/modules/gleam_lang/strings.html
- Exemplos práticos de uso de substrings em Gleam: https://github.com/gleam-lang/stdlib/blob/master/strings/src/String.gleam