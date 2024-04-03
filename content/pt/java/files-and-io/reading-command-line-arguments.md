---
date: 2024-01-20 17:56:13.429214-07:00
description: "Argumentos de linha de comando s\xE3o informa\xE7\xF5es que voc\xEA\
  \ passa para o seu programa Java ao inici\xE1-lo. Programadores usam isso para customizar\
  \ a execu\xE7\xE3o de\u2026"
lastmod: '2024-03-13T22:44:46.471222-06:00'
model: gpt-4-1106-preview
summary: "Argumentos de linha de comando s\xE3o informa\xE7\xF5es que voc\xEA passa\
  \ para o seu programa Java ao inici\xE1-lo."
title: Lendo argumentos da linha de comando
weight: 23
---

## Como Fazer:
Aquela linha de comando clássica `public static void main(String[] args)`? Ela está te dizendo "Ei, me passa alguns argumentos!" Vamos ver isso em ação:

```java
public class LeitorDeArgumentos {
    public static void main(String[] args) {
        if (args.length > 0) {
            System.out.println("Argumentos recebidos:");
            for (String arg : args) {
                System.out.println(arg);
            }
        } else {
            System.out.println("Nenhum argumento foi passado.");
        }
    }
}

```
Executando `java LeitorDeArgumentos esses são argumentos` gera:
```
Argumentos recebidos:
esses
são
argumentos
```

## Aprofundando:
Historicamente, argumentos de linha de comando são tão antigos quanto os próprios computadores pessoais. Eles são o ponto de partida para a interação do usuário com muitos programas em modo texto, desde os tempos do DOS.

Alternativas? Em Java, você pode pedir inputs durante a execução com `Scanner` ou interfaces gráficas. Mas isso é outra conversa.

Quanto à implementação, `args` é um array de `String`. Cada espaço na linha de comando separa os argumentos, e eles são passados para o `args` pela ordem que aparecem.

## Veja Também:
- A documentação oficial da Oracle sobre linhas de comando em Java: [Oracle docs](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
- Tutorial para inicialização de JavaFX que também usa argumentos de linha de comando: [JavaFX](https://docs.oracle.com/javafx/2/get_started/jfxpub-get_started.htm)
- Para uma abordagem mais robusta, olhe para o Apache Commons CLI, que ajuda a parsear argumentos de linha de comandos mais complexos: [Commons CLI](https://commons.apache.org/proper/commons-cli/)
