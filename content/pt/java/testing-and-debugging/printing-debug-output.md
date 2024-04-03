---
date: 2024-01-20 17:52:48.943296-07:00
description: "No mundo da programa\xE7\xE3o, imprimir sa\xEDdas de depura\xE7\xE3\
  o \xE9 o equivalente a uma l\xE2mpada que piscar quando algo errado acontece. \xC9\
  \ extremamente \xFAtil: ajuda a\u2026"
lastmod: '2024-03-13T22:44:46.458662-06:00'
model: gpt-4-1106-preview
summary: "No mundo da programa\xE7\xE3o, imprimir sa\xEDdas de depura\xE7\xE3o \xE9\
  \ o equivalente a uma l\xE2mpada que piscar quando algo errado acontece."
title: "Exibindo sa\xEDdas de depura\xE7\xE3o"
weight: 33
---

## How to:
Vamos partir para o código. Aqui estão alguns exemplos de como exibir mensagens de depuração em Java:

```java
public class DebugExample {

    public static void main(String[] args) {
        
        // Mensagem simples de debug
        System.out.println("Debug: começou a execução do programa.");

        // Debug com uma variável
        int resultado = 42;
        System.out.println("Debug: o resultado calculado foi " + resultado);

        // Usando String.format para uma mensagem formatada
        float valor = 3.14159f;
        System.out.println(String.format("Debug: o valor de PI é aproximadamente %.2f", valor));
        
        // Usando printf para uma mensagem formatada (outra forma)
        System.out.printf("Debug: valor formatado de PI: %.3f%n", valor);

        // Output condicional para depuração
        boolean modoDebug = true;
        if (modoDebug) {
            System.out.println("Debug: modo de depuração ativo!");
        }
    }
}
```

Exemplo de saída:
```
Debug: começou a execução do programa.
Debug: o resultado calculado foi 42
Debug: o valor de PI é aproximadamente 3.14
Debug: valor formatado de PI: 3.142
Debug: modo de depuração ativo!
```

## Deep Dive
Antigamente, no começo da programação, a galera usava sinais luminosos e interruptores para debugar. Hoje, a gente usa o bom e velho `System.out.println()` no Java, mas existem ferramentas mais refinadas como loggers (log4j, SLF4J) que oferecem mais controle sobre o que é impresso e onde isso aparece.

Quando você usa `System.out.println()`, está escrevendo na saída padrão do sistema, que usualmente é o console. Mas e se você precisar de mais controle? Usar um sistema de logging permite configurar níveis de importância, destinos diferentes para as mensagens (como arquivos, bancos de dados, etc.), e formatar as mensagens de maneira mais consistente.

Outra vantagem de usar um sistema de logging é o desempenho. Imprimir mensagens na saída padrão pode ser relativamente lento e afetar o desempenho do seu aplicativo se não for usado com cuidado. Os loggers são mais eficientes e podem ser desativados ou ter sua verbosidade ajustada facilmente, o que é perfeito para a produção.

## See Also
Para entrar mais a fundo nessa história de debug e logging, dá uma olhada nesses links:

- [Documentação da Oracle sobre logging](https://docs.oracle.com/javase/8/docs/technotes/guides/logging/overview.html)
- [SLF4J, um framework de logging](http://www.slf4j.org/)
- [Log4j, outra ferramenta popular de logging](https://logging.apache.org/log4j/2.x/)
- [Tutorial da Baeldung sobre logging em Java](https://www.baeldung.com/java-logging-intro)
