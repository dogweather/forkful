---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:08.209363-07:00
description: "Como fazer: O suporte embutido do Java para regex \xE9 primariamente\
  \ atrav\xE9s das classes `Pattern` e `Matcher` no pacote `java.util.regex`. Aqui\
  \ est\xE1 um\u2026"
lastmod: '2024-03-13T22:44:46.446392-06:00'
model: gpt-4-0125-preview
summary: "O suporte embutido do Java para regex \xE9 primariamente atrav\xE9s das\
  \ classes `Pattern` e `Matcher` no pacote `java.util.regex`."
title: "Usando express\xF5es regulares"
weight: 11
---

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
