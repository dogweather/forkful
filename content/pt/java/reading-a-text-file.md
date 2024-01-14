---
title:    "Java: Lendo um arquivo de texto"
keywords: ["Java"]
---

{{< edit_this_page >}}

# Por Que

Ler e escrever arquivos de texto é uma habilidade fundamental na programação. Isso permite que os desenvolvedores armazenem e manipulem dados de texto de maneira eficiente. Neste post, exploraremos como ler um arquivo de texto em Java, um processo importante para qualquer projeto de programação.

# Como Fazer

Para ler um arquivo de texto em Java, primeiro precisamos criar uma instância da classe File, especificando o caminho do arquivo que queremos ler. Em seguida, criamos uma instância da classe Scanner, que permite ler os dados do arquivo. Se quisermos imprimir o conteúdo do arquivo para o console, podemos usar um loop while para ler cada linha do arquivo e imprimir na tela.

```
Java
File arquivo = new File("caminho/do/arquivo.txt");
Scanner scanner = new Scanner(arquivo);

while (scanner.hasNextLine()) {
    String linha = scanner.nextLine();
    System.out.println(linha);
}

scanner.close();
```

Este código irá imprimir cada linha do arquivo de texto no console.

# Aprofundando-se

Uma consideração importante ao ler um arquivo de texto em Java é o tratamento de exceções. É importante envolver nosso código dentro de um bloco try-catch para lidar com possíveis erros durante a leitura do arquivo.

Outro aspecto importante é o encoding do arquivo de texto. Se o arquivo foi salvo em um encoding diferente do padrão, é necessário especificar isso no construtor da classe Scanner.

# Veja Também

- Documentação oficial do Java sobre leitura de arquivos: https://docs.oracle.com/javase/tutorial/essential/io/file.html
- Tutorial sobre manipulação de arquivos em Java: https://www.baeldung.com/java-file
- Exemplos de código para leitura de arquivos em Java: https://www.programcreek.com/2011/03/java-read-a-file-sample-code/