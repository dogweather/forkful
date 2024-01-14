---
title:                "Java: Lendo um arquivo de texto"
simple_title:         "Lendo um arquivo de texto"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que ler e escrever arquivos de texto em Java

Quando se trata de Java, a leitura e a escrita de arquivos de texto são tarefas bastante comuns. Isso pode ser necessário para armazenar dados em um arquivo ou para a importação e exportação de informações em um programa. Neste post, vamos dar uma olhada em como realizar essas tarefas e aprofundar-nos um pouco mais sobre o processo.

## Como fazer

Para ler e escrever arquivos de texto em Java, precisamos usar as classes FileReader e FileWriter. Vamos começar com a leitura de um arquivo de texto. Primeiro, precisamos criar uma instância da classe FileReader e passar o nome do arquivo como parâmetro. Veja o exemplo abaixo:

```
FileReader arquivo = new FileReader("arquivo.txt");
```

Em seguida, usamos a classe BufferedReader para ler o arquivo linha por linha. Também precisamos de um loop para percorrer todas as linhas do arquivo. Veja o código completo abaixo:

```
try {
    FileReader arquivo = new FileReader("arquivo.txt");
    BufferedReader leitor = new BufferedReader(arquivo);
    String linha;

    while ((linha = leitor.readLine()) != null) {
        System.out.println(linha);
    }

    leitor.close();
} catch (IOException e) {
    System.out.println("Erro na leitura do arquivo: " + e.getMessage());
}
```

Note que usamos um bloco try-catch para capturar quaisquer erros que possam ocorrer durante a leitura do arquivo. Também fechamos o leitor após a leitura para garantir que o arquivo seja liberado e não haja vazamento de memória.

Para escrever em um arquivo de texto, usamos a classe FileWriter da seguinte forma:

```
try {
    FileWriter arquivo = new FileWriter("arquivo.txt");
    BufferedWriter escritor = new BufferedWriter(arquivo);

    escritor.write("Exemplo de texto a ser escrito no arquivo");
    escritor.newLine();
    escritor.write("Outro texto");

    escritor.close();
} catch (IOException e) {
    System.out.println("Erro na escrita do arquivo: " + e.getMessage());
}
```

Assim como na leitura, também usamos um bloco try-catch e fechamos o escritor após a escrita do arquivo.

## Mergulho profundo

Agora que sabemos como ler e escrever arquivos de texto em Java, vamos nos aprofundar um pouco mais no processo de leitura. Quando usamos o método `readLine()` da classe BufferedReader, estamos lendo uma linha inteira do arquivo, incluindo os caracteres de nova linha ("\n"). Para excluir esses caracteres e ter apenas o conteúdo da linha, podemos usar o método `trim()` da classe String. Além disso, a classe File fornece métodos para verificar se o arquivo existe, criar um novo arquivo e obter informações sobre o arquivo, como tamanho e último acesso.

Para a escrita de arquivos, existem outras classes, como PrintWriter e FileOutputStream, que oferecem mais opções de formatação e manipulação dos dados a serem escritos.

## Veja também

- [Documentação oficial do Java sobre FileReader](https://docs.oracle.com/javase/7/docs/api/java/io/FileReader.html)
- [Tutorial da DevMedia sobre manipulação de arquivos em Java](https://www.devmedia.com.br/manipulando-arquivos-com-as-classes-file-e-filewriter-em-java/22673) 
- [Artigo da Alura sobre leitura e escrita de arquivos em Java](https://www.alura.com.br/artigos/lendo-e-escrevendo-arquivos-java)