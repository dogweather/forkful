---
title:                "Imprimindo saída de debug"
html_title:           "C#: Imprimindo saída de debug"
simple_title:         "Imprimindo saída de debug"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/printing-debug-output.md"
---

{{< edit_this_page >}}

# Imprimindo Debug Output em Java: Um Tutorial Simplificado

Este é um guia prático e direto para você, programador, que não tem tempo a perder e precisa de respostas rápidas e eficazes.

## O Que e Por Quê?

Imprimir output de depuração, ou "debug output", é uma técnica usada pelos programadores para monitorar o código durante sua execução e identificar falhas de funcionamento. A prática é valiosa porque facilita a identificação e solução de bugs.

## Como Fazer:

Aqui está um exemplo simples do uso do `System.out.println` para debug:

```
Java
public class DebugExample {
    public static void main(String[] args) {
        String message = "Olá mundo!";
        System.out.println("Debug: " + message);
    }
}
```
Quando o programa for executado, ele imprimirá:
```
Debug: Olá mundo!
```
## Deep Dive:

1. **Contexto histórico:** Desde os primórdios da programação, os desenvolvedores têm usado técnicas de depuração para resolver problemas de código. No Java, o `System.out.println` se tornou uma ferramenta padrão para esse fim;

2. **Alternativas:** À medida que os programas se tornam mais complexos, `System.out.println` pode não ser suficiente. Frameworks de log, como o Log4J ou SLF4J, permitem níveis de log, tags e escrita incondicional para arquivos de log;

3. **Detalhes de implementação:** O `System.out.println` em Java na verdade chama o método `println` da classe `PrintStream`. O `System.out` é um objeto `static` da classe `System` da API Java.

## Veja Também:

- Documentação Oracle sobre a classe System: https://docs.oracle.com/javase/7/docs/api/java/lang/System.html
- Tutorial Log4J: https://www.vogella.com/tutorials/Log4j/article.html
- Documentação do SLF4J: https://www.slf4j.org/manual.html

Lembre-se, a depuração é uma parte fundamental do desenvolvimento. Use essas ferramentas a seu favor, e tornará seu código mais confiável e fácil de manter.