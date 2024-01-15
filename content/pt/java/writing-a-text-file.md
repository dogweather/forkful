---
title:                "Escrevendo um arquivo de texto"
html_title:           "Java: Escrevendo um arquivo de texto"
simple_title:         "Escrevendo um arquivo de texto"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por que escrever um arquivo de texto em Java?

Escrever um arquivo de texto em Java pode ser útil em várias situações, desde armazenar informações em um banco de dados até salvar configurações de um programa. Além disso, pode ser uma forma fácil de criar e armazenar dados que podem ser acessados posteriormente.

## Como fazer:

Para escrever um arquivo de texto em Java, você precisará seguir os seguintes passos:

1. Importe a classe `FileWriter`: Para escrever em um arquivo de texto, você precisará importar a classe `FileWriter` da biblioteca `java.io`.

2. Crie um objeto `FileWriter`: Depois de importar a classe `FileWriter`, você precisa criar um objeto que será responsável por escrever no arquivo. Isso pode ser feito usando o construtor `FileWriter`, que recebe como parâmetro o caminho do arquivo que você deseja criar.

3. Faça a escrita no arquivo: Agora, com o objeto `FileWriter` criado, você pode usar métodos como `write()` ou `append()` para escrever no arquivo. Lembre-se de adicionar "\n" para inserir uma nova linha.

4. Feche o arquivo: Depois de terminar de escrever, é importante fechar o arquivo usando o método `close()`. Isso garantirá que todos os dados sejam gravados corretamente no arquivo.

5. Trate as exceções: É sempre importante tratar as possíveis exceções que podem ocorrer ao escrever em um arquivo, como a ausência de permissões de gravação ou a existência de um arquivo com o mesmo nome.

```java
import java.io.FileWriter;
import java.io.IOException;

public class EscreverArquivo {

    public static void main(String[] args) {

        try {
            // Criando o objeto FileWriter indicando o arquivo que será criado
            FileWriter arquivo = new FileWriter("meuArquivo.txt");

            // Escrevendo no arquivo
            arquivo.write("Este é um arquivo de texto criado em Java!\n");
            arquivo.append("Mais uma linha para o arquivo.");

            // Fechando o arquivo
            arquivo.close();

            System.out.println("Arquivo criado com sucesso!");

        } catch (IOException e) {
            // Tratando possíveis exceções
            System.out.println("Erro ao criar ou escrever no arquivo.");
            e.printStackTrace();
        }
    }
}
```

**Output: meuArquivo.txt**

```
Este é um arquivo de texto criado em Java!
Mais uma linha para o arquivo.
```

## Aprofundamento:

A classe `FileWriter` também possui outros construtores que permitem passar um parâmetro para definir se os novos dados serão sobrescritos ou adicionados ao final do arquivo, além de permitir a especificação do charset para a codificação dos caracteres.

Além disso, é possível utilizar classes como `BufferedReader` e `PrintWriter` para facilitar a escrita e leitura de arquivos de texto em Java. Essas classes oferecem métodos como `readLine()` e `println()` que tornam a manipulação de arquivos mais simples e eficiente.

## Veja também:

- [Documentação da classe FileWriter](https://docs.oracle.com/javase/8/docs/api/java/io/FileWriter.html)
- [Tutorial de Java: Criar, escrever e ler arquivos](https://www.devmedia.com.br/tutorial-java-criando-um-arquivo-txt/25533)