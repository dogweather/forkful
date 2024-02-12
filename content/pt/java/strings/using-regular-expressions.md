---
title:                "Usando expressões regulares"
aliases: - /pt/java/using-regular-expressions.md
date:                  2024-02-03T19:17:08.209363-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando expressões regulares"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Porquê?

Expressões regulares (regex) em Java permitem definir padrões específicos para pesquisar, manipular ou validar strings no seu código. Programadores as utilizam para tarefas como análise de arquivos de log, validação de entrada de usuário, ou procura por padrões específicos dentro de textos, possibilitando um processamento sofisticado de strings com esforço mínimo.

## Como fazer:

O suporte embutido do Java para regex é primariamente através das classes `Pattern` e `Matcher` no pacote `java.util.regex`. Aqui está um exemplo simples para encontrar e imprimir todas as ocorrências de uma palavra em uma string, ignorando maiúsculas e minúsculas:

```java
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RegexExample {
    public static void main(String[] args) {
        String text = "Regex é ótimo para análise. Análise com regex é poderosa.";
        String palavraParaEncontrar = "análise";
        
        Pattern pattern = Pattern.compile(palavraParaEncontrar, Pattern.CASE_INSENSITIVE);
        Matcher matcher = pattern.matcher(text);
        
        while (matcher.find()) {
            System.out.println("Encontrado '" + matcher.group() + "' na posição " + matcher.start());
        }
    }
}
```

Saída:
```
Encontrado 'análise' na posição 16
Encontrado 'Análise' na posição 31
```

Para tarefas como dividir strings, você pode usar o método `split()` da classe `String` com um regex:

```java
public class SplitExample {
    public static void main(String[] args) {
        String text = "Java,Python,Ruby,JavaScript";
        String[] linguagens = text.split(",");
        
        for (String linguagem : linguagens) {
            System.out.println(linguagem);
        }
    }
}
```

Saída:
```
Java
Python
Ruby
JavaScript
```

Quando trabalhando com regex em Java, pode haver casos onde uma biblioteca externa pode simplificar tarefas complexas. Uma das bibliotecas de terceiros populares para trabalhar com regex em Java é a `Apache Commons Lang`. Ela oferece utilitários como `StringUtils` que tornam algumas tarefas de regex mais diretas. Aqui está como usá-la para contar correspondências de uma substring:

```java
import org.apache.commons.lang3.StringUtils;

public class CommonsLangExample {
    public static void main(String[] args) {
        String text = "Regex torna o processamento de texto mais fácil. Processar texto com regex é eficiente.";
        String substring = "processamento";
        
        int count = StringUtils.countMatches(text, substring);
        System.out.println("'" + substring + "' aparece " + count + " vezes.");
    }
}
```

Para usar a Apache Commons Lang, você precisa incluí-la no seu projeto. Se você está usando Maven, adicione esta dependência ao seu `pom.xml`:

```xml
<dependency>
    <groupId>org.apache.commons</groupId>
    <artifactId>commons-lang3</artifactId>
    <version>3.12.0</version> <!-- Verifique a versão mais recente -->
</dependency>
```

Saída:
```
'processamento' aparece 2 vezes.
```
