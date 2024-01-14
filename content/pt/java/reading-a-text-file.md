---
title:                "Java: Lendo um arquivo de texto"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que ler um arquivo de texto em Java?

Ler um arquivo de texto é uma tarefa comum na programação em Java. É útil para uma variedade de aplicações, desde ler e processar arquivos de dados até ler e mostrar arquivos de texto simples para o usuário. Neste post, vamos discutir como ler um arquivo de texto em Java e suas diferentes aplicações.

## Como fazer
Utilizando a classe Java *BufferedReader*, podemos ler um arquivo de texto linha por linha. Primeiro, precisamos criar uma instância da classe *File* e passar o caminho do arquivo como parâmetro. Depois, podemos usar essa instância para criar um objeto *BufferedReader* e ler as linhas do arquivo. Veja um exemplo abaixo:

```Java

// Criando a instância do arquivo
File arquivo = new File("caminho/do/arquivo.txt");

// Criando o objeto BufferedReader
BufferedReader leitor = new BufferedReader(new FileReader(arquivo));

// Lendo as linhas do arquivo
String linha;
while ((linha = leitor.readLine()) != null) {
    System.out.println(linha); // Imprime a linha lida
}

// Fechando o leitor e tratando exceções
leitor.close();
```

O resultado da execução do código acima mostrará as linhas do arquivo de texto na saída do console. É importante notar que o método *readLine()* retorna um objeto *String*, então podemos fazer qualquer operação com a linha lida.

## Profundidade na leitura de arquivos de texto
Além de apenas ler e mostrar as linhas do arquivo de texto, é possível realizar diversas operações e manipulações mais avançadas. Podemos, por exemplo, criar um arquivo de texto a partir de uma lista de strings ou procurar por uma determinada palavra em um arquivo de texto.

Um ponto importante é que, quando manipulamos arquivos, é necessário tratar possíveis exceções que possam ocorrer. Para isso, podemos utilizar blocos *try-catch* para lidar com erros, como arquivos inexistentes ou permissões de acesso negadas.

## Veja também
- Documentação oficial do Java sobre leitura de arquivos: [https://docs.oracle.com/javase/tutorial/essential/io/file.html](https://docs.oracle.com/javase/tutorial/essential/io/file.html)
- Tutorial completo sobre leitura de arquivos em Java: [https://www.baeldung.com/java-read-lines-large-file](https://www.baeldung.com/java-read-lines-large-file)
- Exemplos práticos de leitura de arquivos em Java: [https://www.geeksforgeeks.org/file-handling-java-using-filewriter-filereader/](https://www.geeksforgeeks.org/file-handling-java-using-filewriter-filereader/)