---
title:                "Utilizando expressões regulares"
date:                  2024-01-19
html_title:           "Bash: Utilizando expressões regulares"
simple_title:         "Utilizando expressões regulares"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Regular expressions, ou regex, são padrões usados para encontrar correspondências em strings. Programadores usam regex para validação de dados, busca e substituição, e análise de texto - é como ter um canivete suíço para lidar com texto.

## Como Fazer:

```java
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class ExemploRegex {

    public static void main(String[] args) {
        // Define o padrão regex para encontrar dígitos
        Pattern padrao = Pattern.compile("\\d+");
        // Cria um objeto Matcher para encontrar correspondências na string
        Matcher correspondente = padrao.matcher("O ano é 2023");

        // Verifica se há correspondências
        while (correspondente.find()) {
            System.out.println("Número encontrado: " + correspondente.group());
        }
    }
}
```

Output:

```
Número encontrado: 2023
```

## Aprofundamento

Regex existe desde os anos 50 e tornou-se padrão na maioria das linguagens de programação. Alternativas ao uso de regex incluem parseres específicos de linguagem, como ANTLR, ou APIs de processamento de strings. Em Java, a classe `Pattern` compila a expressão regular em uma representação interna otimizada para a pesquisa rápida, enquanto a classe `Matcher` executa as operações de correspondência.

## Veja Também

- Documentação oficial da classe `Pattern` em Java: https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/regex/Pattern.html
- Documentação oficial da classe `Matcher` em Java: https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/regex/Matcher.html
- Tutorial interativo de regex: https://regexr.com/
- Livro "Mastering Regular Expressions" para uma compreensão mais aprofundada das expressões regulares e seus usos.
