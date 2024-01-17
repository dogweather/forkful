---
title:                "Excluindo caracteres que correspondem a um padrão"
html_title:           "Java: Excluindo caracteres que correspondem a um padrão"
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

O que é e por que excluir caracteres que correspondem a um padrão:
Excluir caracteres que correspondem a um padrão é uma técnica de programação que permite aos desenvolvedores remover caracteres específicos de uma string seguindo um determinado padrão. Isso pode ser útil em situações em que é necessário limpar ou formatar dados de entrada, ou quando determinados caracteres precisam ser removidos da string para atender a um requisito específico.

Como fazer:
A exclusão de caracteres que correspondem a um padrão pode ser facilmente implementada em Java usando expressões regulares. O código a seguir mostra como remover todas as vogais de uma string:

```java
String original = "Programação em Java é divertido!";
String semVogais = original.replaceAll("[aeiouAEIOU]", ""); // o método replaceAll substitui todas as ocorrências do padrão pela string vazia
System.out.println(semVogais);
// Output: Prgrmçm m Jv é dvrtd!
```

Para remover apenas o primeiro caractere que corresponde ao padrão, você pode usar o método replaceFirst em vez de replaceAll.

Mergulho profundo:
As expressões regulares são uma técnica poderosa para manipulação de strings que foram introduzidas pela primeira vez na linguagem de programação Perl em 1987. Elas são amplamente utilizadas em várias linguagens de programação, incluindo Java, para manipulação de texto sofisticada. Expressões regulares permitem que você defina padrões de texto complexos e pesquise, substitua ou extraia dados de strings de maneira eficiente.

Existem várias alternativas para a exclusão de caracteres que correspondem a um padrão em Java. Além do método replaceAll mencionado anteriormente, você também pode usar o método replace para substituir um caractere específico, ou o método delete para remover um intervalo de caracteres.

OK, veja também:
Links para fontes relacionadas:

- A documentação oficial da classe String Java: https://docs.oracle.com/javase/8/docs/api/java/lang/String.html
- Um tutorial detalhado sobre expressões regulares em Java: https://www.vogella.com/tutorials/JavaRegularExpressions/article.html