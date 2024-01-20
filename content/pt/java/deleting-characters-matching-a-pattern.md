---
title:                "Excluindo caracteres que correspondem a um padrão"
html_title:           "Arduino: Excluindo caracteres que correspondem a um padrão"
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Deletar caracteres com um padrão específico é uma técnica utilizada para eliminar ou modificar pedaços de texto (strings) que correspondam a um determinado critério. É muito utilizada em programação para limpar dados, validar entrada do usuário, entre outros.

## Como Fazer:
Aqui está um breve exemplo de como remover caracteres que correspondem a um determinado padrão em Java:

```java
import java.util.regex.*;

public class Main {
    public static void main(String[] args) {
        String str = "ABC123XYZ";
        String padrao = "[0-9]";  // Vamos remover todos os números

        Pattern p = Pattern.compile(padrao);
        Matcher m = p.matcher(str);

        String resultado = m.replaceAll("");

        System.out.println(resultado);  // Vai imprimir "ABCXYZ"
    }
}
```

Neste exemplo, removemos todos os números da string "ABC123XYZ", resultando em "ABCXYZ".

## Mergulho Profundo

O suporte a expressões regulares foi adicionado no Java 1.4, permitindo aos programadores pesquisar e manipular strings de maneira poderosa e flexível. Antes disso, a remoção de caracteres que correspondem a um padrão geralmente exigia loops e lógica condicional personalizada.

Existem várias outras maneiras de remover caracteres que correspondem a um padrão em Java, dependendo de suas necessidades específicas. Por exemplo, você pode usar o método `.replace()` ou `.replaceFirst()` de uma string se souber exatamente o que deseja remover.

No entanto, esse método `replaceAll()` em combinação com expressões regulares é o mais poderoso e flexível. Ele permite que você defina um padrão complexo que os caracteres devem corresponder e pode substituir todas as ocorrências desse padrão por uma string de substituição, tudo em uma única linha de código.

## Veja Também

* Tutorial de Regex em Java: https://www.vogella.com/tutorials/JavaRegularExpressions/article.html
* Manipulação de Strings em Java: https://www.geeksforgeeks.org/string-handling-java/