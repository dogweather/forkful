---
title:                "Capitalizando uma string"
date:                  2024-01-19
html_title:           "Bash: Capitalizando uma string"
simple_title:         "Capitalizando uma string"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Capitalizar uma string significa transformar todas as letras do texto em maiúsculas. Programadores utilizam esse recurso para padronizar entradas de dados, destacar informações importantes ou atender a requisitos estéticos e de formato.

## Como Fazer:

Para capitalizar strings em Java, podemos usar o método `toUpperCase()` da classe `String`. Simples e direto:

```java
public class CapitalizeExample {
    public static void main(String[] args) {
        String original = "programação em java";
        String capitalized = original.toUpperCase();

        System.out.println(capitalized); // Output: PROGRAMAÇÃO EM JAVA
    }
}
```

Se executarmos esse código, a saída será:

```
PROGRAMAÇÃO EM JAVA
```

## Mergulho Profundo:

Historicamente, a necessidade de capitalizar strings vem dos tempos de computadores de cartão perfurado e terminais que suportavam apenas letras maiúsculas. Embora hoje tenhamos suporte completo para minúsculas, a prática de capitalizar continua por questões de uniformidade e legibilidade.

Alternativas para capitalização incluem o uso de bibliotecas de terceiros, como Apache Commons Lang, que oferece métodos como `StringUtils.capitalize()`, que torna apenas a primeira letra de cada palavra maiúscula.

No tocante a detalhes de implementação, o método `toUpperCase()` pode comportar-se de maneira diferente dependendo do `Locale` utilizado, pois algumas linguagens têm regras específicas para transformação de maiúsculas e minúsculas.

Exemplo com `Locale`:

```java
import java.util.Locale;

public class LocaleCapitalizeExample {
    public static void main(String[] args) {
        String original = "flor naïve";
        String capitalizedUS = original.toUpperCase(Locale.US); // Considera inglês dos EUA
        String capitalizedTR = original.toUpperCase(new Locale("tr", "TR")); // Considera turco

        System.out.println(capitalizedUS); // Output: FLOR NAÏVE
        System.out.println(capitalizedTR); // Output: FLOR NAİVE (Pontuação alterada)
    }
}
```

Note que o caracter 'i' é capitalizado diferentemente dependendo do `Locale`.

## Veja Também:

- [Classe String na documentação oficial do Java](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html)
- [Apache Commons Lang StringUtils](https://commons.apache.org/proper/commons-lang/apidocs/org/apache/commons/lang3/StringUtils.html)
- [Localização e internacionalização em Java](https://docs.oracle.com/javase/tutorial/i18n/)
