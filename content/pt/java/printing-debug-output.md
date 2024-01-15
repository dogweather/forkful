---
title:                "Imprimindo saída de depuração"
html_title:           "Java: Imprimindo saída de depuração"
simple_title:         "Imprimindo saída de depuração"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que 

Você já passou horas tentando descobrir por que o seu código não está funcionando corretamente? Ou se deparou com um bug que parece impossível de solucionar? Às vezes, a melhor maneira de entender o que está acontecendo é imprimindo mensagens de debug no seu código. Isso pode te ajudar a identificar problemas e encontrar a solução mais rapidamente.

## Como Fazer

Para imprimir mensagens de debug em Java, você pode utilizar o método "System.out.print()" ou "System.out.println()". Vamos ver alguns exemplos:

```Java
int a = 5;
int b = 7;

// Imprimindo o valor da variável 'a'
System.out.println("O valor de a é: " + a); // Saída: O valor de a é: 5

// Imprimindo uma mensagem de debug
System.out.println("Debug: O valor de a é: " + a); // Saída: Debug: O valor de a é: 5

// Imprimindo a soma de duas variáveis
int soma = a + b;
System.out.println("A soma de a e b é: " + soma); // Saída: A soma de a e b é: 12
```

Além disso, você também pode utilizar o método "System.out.printf()" para imprimir mensagens formatadas, dando mais clareza e organização aos seus outputs. Veja um exemplo:

```Java
String nome = "João";
int idade = 25;

System.out.printf("O nome é %s e a idade é %d anos.", nome, idade); // Saída: O nome é João e a idade é 25 anos.
```

## Mergulho Profundo

Agora que já vimos como imprimir mensagens de debug, é importante entender quando e onde utilizar esse recurso. Uma boa prática é adicionar essas mensagens em pontos chave do seu código, como no início e no fim de um método ou em trechos que possam ser suspeitos de causar problemas. Além disso, é importante lembrar de remover essas mensagens antes de fazer o deploy da sua aplicação, evitando assim clutter (desordem) no seu código.

Outra técnica útil é utilizar uma flag booleana para habilitar ou desabilitar a impressão de mensagens de debug. Assim, você pode facilmente ligar e desligar essas mensagens de acordo com a necessidade, sem precisar apagar ou adicionar os comandos de impressão a todo momento.

## Veja também

- [Documentação oficial do Java sobre System.out.println()](https://docs.oracle.com/javase/7/docs/api/java/io/PrintStream.html#println())
- [Tutorial sobre Debugging em Java](https://www.baeldung.com/java-debugging)
- [Artigo sobre Boas Práticas de Debugging em Java](https://stackabuse.com/debugging-in-java-best-practices/)