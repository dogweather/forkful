---
date: 2024-01-20 17:57:56.624551-07:00
description: "How to: Em Java, voc\xEA pode usar o m\xE9todo `replace()` ou `replaceAll()`\
  \ da classe `String`. Aqui vai um exemplo r\xE1pido."
lastmod: '2024-03-13T22:44:46.441801-06:00'
model: gpt-4-1106-preview
summary: "Em Java, voc\xEA pode usar o m\xE9todo `replace()` ou `replaceAll()` da\
  \ classe `String`."
title: Pesquisando e substituindo texto
weight: 10
---

## How to:
Em Java, você pode usar o método `replace()` ou `replaceAll()` da classe `String`. Aqui vai um exemplo rápido:

```java
public class SearchAndReplace {
    public static void main(String[] args) {
        String originalText = "As raposas são astutas e rápidas.";
        String newText = originalText.replace("raposas", "gatos");
        System.out.println(newText);
        
        String regexText = originalText.replaceAll("rápid(.)s", "lent$1s");
        System.out.println(regexText);
    }
}
```

Saída do código:
```
As gatos são astutas e rápidas.
As raposas são astutas e lentas.
```

## Deep Dive
Histórico: A prática de procurar e trocar texto é tão antiga quanto a própria programação. Inicialmente, isso era feito manualmente em editores de texto até que ferramentas como `sed` do UNIX começaram a automatizar o processo.

Alternativas: Além dos métodos `replace()` e `replaceAll()`, você pode usar a classe `Pattern` e `Matcher` para procurar e substituir utilizando expressões regulares (regex) de forma mais controlada.

Detalhes de Implementação: O método `replace()` substitui todas as ocorrências do texto literalmente, enquanto o `replaceAll()` considera a string de substituição como uma expressão regular.

## See Also
- [Class String Documentation](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html)
- [Java Regex Tutorial](https://www.vogella.com/tutorials/JavaRegularExpressions/article.html)
- [UNIX sed Command](https://www.gnu.org/software/sed/manual/sed.html)
