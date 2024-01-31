---
title:                "Pesquisando e substituindo texto"
date:                  2024-01-20T17:57:56.624551-07:00
model:                 gpt-4-1106-preview
simple_title:         "Pesquisando e substituindo texto"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?
Procurar e substituir texto é uma prática de encontrar sequências específicas de caracteres numa string e trocá-las por outras. Programadores fazem isso para corrigir erros, atualizar informações ou automatizar edições em massa.

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
