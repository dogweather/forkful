---
title:    "Java: Lendo um arquivo de texto."
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que ler um arquivo de texto em Java?

Ler um arquivo de texto é uma tarefa comum em muitos projetos de programação Java. Isso pode ser necessário para ler dados de entrada, configurações do programa ou mesmo para processar grandes volumes de dados. Neste artigo, vamos aprender como ler um arquivo de texto em Java e também mergulhar mais fundo nas melhores práticas para essa tarefa.

## Como ler um arquivo de texto em Java

Ler um arquivo de texto em Java é um processo simples e pode ser feito em poucas linhas de código. Primeiro, precisamos importar a classe `java.io.File` que nos permitirá representar o arquivo que queremos ler. Em seguida, usamos a classe `Scanner` para ler os dados do arquivo linha por linha. Aqui está um exemplo de código que lê um arquivo chamado `texto.txt` e imprime seu conteúdo:

```java
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class LerArquivoTexto {
    public static void main(String[] args) {
        // Criar um objeto File para o arquivo texto.txt
        File arquivo = new File("texto.txt");

        try {
            // Criar um objeto Scanner para ler o arquivo
            Scanner scanner = new Scanner(arquivo);

            // Iterar pelas linhas do arquivo
            while (scanner.hasNextLine()) {
                // Ler a próxima linha e imprimir na tela
                System.out.println(scanner.nextLine());
            }

            scanner.close(); // Fechar o scanner
        } catch (FileNotFoundException e) {
            System.out.println("O arquivo não foi encontrado.");
        }
    }
}
```

A saída desse código será o conteúdo do arquivo `texto.txt` impresso na tela. Note que é importante fechar o objeto `Scanner` após a leitura do arquivo para evitar vazamentos de recursos.

## Mais informações sobre leitura de arquivos de texto

Quando estamos lidando com grandes arquivos de texto, é importante ter cuidado com a performance do nosso código. Uma maneira de otimizar a leitura é usar a classe `BufferedReader`, que armazena o conteúdo do arquivo em um buffer antes de ser lido. Isso pode ser especialmente útil para processar arquivos com milhares de linhas.

Outro ponto importante é tratar as exceções corretamente. Quando lidamos com arquivos, sempre há a possibilidade de ocorrer um erro, seja porque o arquivo não existe ou porque não temos permissão para lê-lo. Certifique-se de sempre tratar essas exceções de forma apropriada em seu código.

## Veja também

Aqui estão alguns links úteis para aprender mais sobre a leitura de arquivos de texto em Java:

- [Documentação oficial do Java sobre leitura de arquivos](https://docs.oracle.com/javase/tutorial/essential/io/file.html)
- [Tutorial sobre leitura de arquivos em Java](https://www.baeldung.com/java-read-lines-large-file)
- [Exemplos de código para ler arquivos em Java](https://www.journaldev.com/709/java-read-file-line-by-line)

Com essas informações, você estará pronto para lidar com qualquer tipo de arquivo de texto em seus projetos Java. Pratique e experimente diferentes abordagens para encontrar a que melhor se adapta às suas necessidades. Boa sorte!