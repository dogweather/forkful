---
title:    "Java: Escrevendo no erro padrão"
keywords: ["Java"]
---

{{< edit_this_page >}}

# Por que escrever para o fluxo de erro padrão é importante?

Escrever para o fluxo de erro padrão é importante porque é uma forma de lidar com erros e exceções em seu código. Quando um programa é executado, ele pode encontrar erros e falhas que precisam ser tratados para que o programa possa continuar sua execução corretamente. O fluxo de erro padrão permite que você identifique e lide com esses problemas de forma adequada.

## Como fazer isso em Java

Em Java, podemos usar o método `System.err.println()` para escrever mensagens de erro no fluxo de erro padrão. Vamos dar uma olhada em um exemplo simples:

```Java
public class ErroPadrao {
  public static void main(String[] args) {
    int a = 10;
    int b = 0;

    try {
        int resultado = a/b;
        System.out.println("O resultado é: " + resultado);
    } catch(ArithmeticException e) {
        System.err.println("Erro: Divisão por zero!");
    }
  }
}
```

Nesse código, estamos tentando dividir o número `10` por `0`, o que resultará em uma exceção `ArithmeticException`. Dentro do bloco `catch`, usamos o método `System.err.println()` para imprimir uma mensagem de erro no fluxo de erro padrão. Isso nos permite saber o que causou o erro e lidar com ele.

## Explorando mais sobre o fluxo de erro padrão

Escrever para o fluxo de erro padrão também é útil para depuração de código. Em vez de interromper a execução do programa, você pode escrever mensagens de erro no fluxo de erro padrão para entender o que está acontecendo em diferentes partes do seu código.

Além disso, é possível redirecionar o fluxo de erro padrão para um arquivo, permitindo que você salve essas mensagens de erro para referência futura. Isso é especialmente útil em aplicativos onde é importante registrar e analisar erros.

# Veja também
- Documentação oficial do Java para System.err: https://docs.oracle.com/javase/8/docs/api/java/lang/System.html#err
- Tutorial do W3Schools sobre exceções em Java: https://www.w3schools.com/java/java_try_catch.asp
- Artigo sobre como redirecionar o fluxo de erro padrão em Java: https://www.baeldung.com/java-write-to-system-error

*Este artigo foi escrito para leitores que falam português brasileiro. Se você fala outro idioma, por favor, consulte seus recursos de aprendizagem em sua língua nativa para obter informações sobre escrever para o fluxo de erro padrão em Java.*