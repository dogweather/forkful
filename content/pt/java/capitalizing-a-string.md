---
title:                "Colocando em Maiúsculo uma String"
html_title:           "Java: Colocando em Maiúsculo uma String"
simple_title:         "Colocando em Maiúsculo uma String"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## O que e por que?

Capitalizar uma string em Java é o processo de converter a primeira letra de cada palavra em maiúscula. Programadores fazem isso para melhorar a legibilidade e organização do código. 

## Como fazer:

```Java
public static String capitalize(String str) {
    if (str == null || str.isEmpty()) {
        return str;
    } else {
        String[] words = str.split("\\s+");
        StringBuilder sb = new StringBuilder();
        for (String word : words) {
            sb.append(Character.toUpperCase(word.charAt(0)))
              .append(word.substring(1).toLowerCase())
              .append(" ");
        }
        return sb.toString().trim();
    }
}

public static void main(String[] args) {
    System.out.println(capitalize("este é um exemplo de string capitalizada"));
}
```

Output: Este é um exemplo de string capitalizada.

## Aprofundamento:

Capitalizar strings é uma prática comum em muitas linguagens de programação, com o objetivo de tornar o texto mais legível e organizado. Em português, é conhecida como "título" ou "sentença capitalizada", e é frequentemente usada em títulos, cabeçalhos e outros textos que precisam ser destacados. Existem também outras formas de capitalização, como todas as letras maiúsculas (UPPER CASE) ou todas as letras minúsculas (lower case).

## Veja também:

- [String Class in Java](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html)
- [Capitalize Method in Java](https://docs.oracle.com/javase/7/docs/api/java/lang/Character.html#toUpperCase(int))