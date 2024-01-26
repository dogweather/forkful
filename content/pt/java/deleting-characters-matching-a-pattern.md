---
title:                "Excluindo caracteres que correspondem a um padrão"
date:                  2024-01-20T17:42:37.886022-07:00
model:                 gpt-4-1106-preview
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why?
Deletar caracteres que correspondem a um padrão é basicamente tirar do texto tudo o que a gente não quer ou não precisa. Programadores fazem isso para limpar dados, validar inputs ou simplificar strings para processamento.

## How to:
Em Java, podemos usar a classe `Pattern` e a classe `Matcher` para localizar e deletar os trechinhos específicos dentro de uma string. Vamos ver como isso funciona.

```java
import java.util.regex.Pattern;
import java.util.regex.Matcher;

public class DeletePatternExample {
    public static void main(String[] args) {
        String originalString = "abacaxi99 é uma fruta100 deliciosa";
        String patternString = "\\d+"; // padrão regex para encontrar números

        Pattern pattern = Pattern.compile(patternString);
        Matcher matcher = pattern.matcher(originalString);

        String cleanedString = matcher.replaceAll("");
        System.out.println(cleanedString);
    }
}
```
Saída do exemplo:
```
abacaxi é uma fruta deliciosa
```
Repare que todos os números foram removidos da string original.

## Deep Dive
Deletar caracteres seguindo um padrão não é uma tecnologia nova; isso já está em diversas linguagens de programação há décadas, através de expressões regulares, ou regex. Regex é poderoso, mas pode ser complicadinho no começo. Existem alternativas, como usar o método `replace()` ou `replaceAll()` de `String` para substituir sem regex, mas são menos flexíveis. Quanto à implementação, o Java compila o padrão regex numa série de instruções que procuram correspondências na string — se liga que isso pode ser menos eficiente se usado indevidamente, especialmente em loops.

## See Also
- [Classe Pattern na documentação oficial do Java](https://docs.oracle.com/javase/10/docs/api/java/util/regex/Pattern.html)
- [Tutorial de Expressões Regulares em Java](https://www.vogella.com/tutorials/JavaRegularExpressions/article.html)
