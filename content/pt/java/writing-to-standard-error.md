---
title:                "Escrevendo para o erro padrão"
html_title:           "Java: Escrevendo para o erro padrão"
simple_title:         "Escrevendo para o erro padrão"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que escrever para o erro padrão?

Muitas vezes, durante o processo de desenvolvimento de um programa em Java, podem ocorrer erros que precisam ser identificados e corrigidos. Ao escrever mensagens de erro para o erro padrão, podemos ter uma noção mais clara da origem do problema e, assim, facilitar a resolução do mesmo.

## Como fazer isso?

Para escrever para o erro padrão em Java, usamos o objeto "System.err". Podemos usar os métodos "println()" e "print()" para imprimir uma mensagem no erro padrão, seguido pelo sinal de exclamação (!) para garantir que a mensagem seja exibida.

```
Java System.err.println("Mensagem de erro!"); 
```

Ao rodar esse código, teremos a mensagem de erro impressa no console do usuário.

## Mergulho Profundo

Além de imprimir mensagens de erro simples, podemos utilizar a classe "PrintWriter" para escrever log de erros em um arquivo para posterior análise. Isso pode ser útil quando precisamos rastrear e corrigir erros em aplicações mais complexas.

```
Java PrintWriter pw = new PrintWriter(new FileWriter("log.txt"));
pw.println("Erro encontrado!");
pw.close();
```

Aqui, criamos um objeto PrintWriter que irá escrever em um arquivo de texto chamado "log.txt". Em seguida, utilizamos o método "println()" para escrever a mensagem de erro no arquivo e, por fim, fechamos o objeto PrintWriter. O resultado será um arquivo de log com uma lista de erros encontrados durante a execução do programa.

## Veja também

- [Documentação Oficial do Java](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html#err)
- [Artigo sobre tratamento de erros em Java](https://www.devmedia.com.br/tratamento-de-erros-em-java/22101)