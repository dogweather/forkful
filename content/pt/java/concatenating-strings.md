---
title:                "Concatenando strings"
date:                  2024-01-20T17:35:05.297050-07:00
model:                 gpt-4-1106-preview
simple_title:         "Concatenando strings"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?
Concatenar strings é juntar duas ou mais sequências de caracteres. Programadores fazem isso para montar mensagens, unir dados e gerar saídas dinâmicas.

## How to:
Em Java, você pode concatenar strings de várias formas. Aqui estão dois jeitos populares:

```java
public class ConcatDemo {
    public static void main(String[] args) {
        // Usando o operador +
        String hello = "Olá";
        String world = "Mundo";
        String greeting = hello + ", " + world + "!";
        System.out.println(greeting); // Saída: Olá, Mundo!

        // Usando StringBuilder para múltiplas concatenações
        StringBuilder sb = new StringBuilder();
        sb.append(hello).append(", ").append(world).append("!");
        System.out.println(sb.toString()); // Saída: Olá, Mundo!
    }
}
```

## Deep Dive
A concatenação de strings em Java vem desde o início. No entanto, a maneira como é implementada teve algumas mudanças. Usar o operador `+` é simples e lê-se bem, mas em loops ou concatenações frequentes, um `StringBuilder` é mais eficiente porque evita a criação de muitos objetos string intermediários.

Alternativas incluem `StringBuffer` (thread-safe, mas mais lento) e o método `concat()` da classe `String` (raramente usado, visto que o operador `+` é mais prático).

Antigamente, a concatenação com operador `+` em loops era desaconselhada por razões de desempenho, mas as versões mais recentes do Java fazem otimizações sob o capô, usando `StringBuilder` automaticamente em expressões concatenadas.

## See Also
- [Java String documentation](https://docs.oracle.com/en/java/javase/18/docs/api/java.base/java/lang/String.html)
- [StringBuilder documentation](https://docs.oracle.com/en/java/javase/18/docs/api/java.base/java/lang/StringBuilder.html)
- [Effective Java Item 63: Beware the performance of string concatenation](https://www.effectivejava.com/)
