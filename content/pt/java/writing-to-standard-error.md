---
title:                "Escrevendo no erro padrão"
date:                  2024-01-19
html_title:           "Arduino: Escrevendo no erro padrão"
simple_title:         "Escrevendo no erro padrão"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## O Que é & Por Que?

Escrever no erro padrão (`System.err`) significa enviar mensagens de erro para um fluxo separado do fluxo de saída normal (`System.out`). Isso permite que você diferencie os erros do output regular do seu programa, tornando mais fácil a depuração e o log de problemas.

## How to:

O Java torna incrivelmente simples escrever no erro padrão. Aqui estão exemplos de como fazê-lo:

```java
public class ErroPadraoExemplo {
    public static void main(String[] args) {
        System.err.println("Oops! Algo deu errado.");
        int[] numeros = new int[2];
        try {
            int resultado = numeros[2];
        } catch (ArrayIndexOutOfBoundsException e) {
            System.err.println("Erro: " + e.getMessage());
        }
    }
}
```

Output esperado no console:

```plaintext
Oops! Algo deu errado.
Erro: Index 2 out of bounds for length 2
```

## Deep Dive

Historicamente, a divisão entre erro padrão e saída padrão remonta aos primeiros dias dos sistemas Unix, permitindo que programas em pipeline tratassem erros de forma independente do output normal. Alternativamente, programadores podem usar logging frameworks como o Log4J ou o SLF4J para gerenciar mensagens de erro, que oferecem funcionalidades adicionais como níveis de log e redirecionamento para arquivos. Em Java, a escrita no erro padrão é implementada nativamente, mas, se necessário, é possível reatribuir `System.err` para outro `PrintStream`.

## See Also

Para mais informações e práticas recomendadas, veja os seguintes recursos:

- [Official Java Documentation - System.err](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/System.html)
- [Log4J – Apache Logging Services](https://logging.apache.org/log4j/2.x/)
- [SLF4J - Simple Logging Facade for Java](http://www.slf4j.org/)
