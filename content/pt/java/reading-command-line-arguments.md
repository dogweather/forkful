---
title:                "Java: Lendo argumentos da linha de comando"
simple_title:         "Lendo argumentos da linha de comando"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que?

Se você está aprendendo a programar em Java ou já possui algum conhecimento na linguagem, provavelmente já ouviu falar em "argumentos de linha de comando". Mas afinal, por que alguém se interessaria em ler esses argumentos?

Os argumentos de linha de comando são valores que podem ser passados para um programa no momento em que ele é executado. Eles permitem que o usuário personalize a execução do programa, fornecendo informações importantes ou alterando seu comportamento. Portanto, entender como ler esses argumentos é essencial para criar programas mais flexíveis e interativos.

## Como fazer?

Java possui uma classe chamada "Main" que é a ponto de entrada para um programa. É nela que você deve incluir o método "main", responsável por iniciar a execução do programa. Para ler os argumentos da linha de comando, você também precisará utilizar outro parâmetro chamado "String[] args".

Um exemplo de como fazer isso seria:

```Java
public static void main(String[] args){
   if(args.length > 0){ //verifica se há argumentos passados
      System.out.println("Os argumentos passados foram: ");
      for(String arg: args){
         System.out.println(arg); //imprime cada argumento na tela
      }
   }
}
```
Ao executar esse programa no terminal, você pode passar os argumentos separando-os por espaços. Por exemplo: `java Main argumento1 argumento2 argumento3`. O output seria:

```Java
Os argumentos passados foram:
argumento1
argumento2
argumento3
```

É importante lembrar que os argumentos passados são sempre tratados como Strings. Se você precisar convertê-los para outros tipos de dados, como int ou float, deverá fazê-lo manualmente utilizando os métodos de conversão disponíveis em Java.

## Mergulhando mais fundo

Além do exemplo básico apresentado acima, é possível fazer uma análise mais aprofundada dos argumentos de linha de comando em Java. Por exemplo, você pode utilizar a classe "Scanner" para ler os argumentos como entrada do usuário, permitindo mais interatividade no programa. Além disso, existem bibliotecas e frameworks que facilitam a leitura e o processamento dos argumentos, como o Apache Commons CLI.

Também é importante mencionar que a posição dos argumentos na linha de comando é importante. O primeiro argumento sempre ocupará a posição 0 no array "args", o segundo ocupará a posição 1 e assim por diante. Portanto, é fundamental ter conhecimento sobre a ordem em que os argumentos devem ser passados para que o programa funcione corretamente.

## Veja também

- [Documentação oficial do Java sobre argumentos de linha de comando](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
- [Exemplos práticos de leitura de argumentos de linha de comando em Java](https://stackabuse.com/command-line-arguments-in-java/)
- [Tutorial completo sobre como utilizar a classe Scanner em Java](https://www.w3schools.com/java/java_user_input.asp)