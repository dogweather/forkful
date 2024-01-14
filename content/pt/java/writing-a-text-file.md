---
title:                "Java: Escrevendo um arquivo de texto"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por que escrever um arquivo de texto em Java?

Escrever um arquivo de texto pode ser necessário em diversas situações. Pode ser para armazenar dados importantes, gerar relatórios ou salvar informações para serem utilizadas posteriormente. Através da programação em Java, é possível criar e manipular arquivos de texto de forma muito fácil e eficiente.

## Como fazer isso em Java

Para começar a escrever um arquivo de texto em Java, é necessário importar a classe `java.io.FileWriter` e tratar as possíveis exceções que possam ocorrer durante o processo de escrita do arquivo. Veja um exemplo de código abaixo:

``` Java 
import java.io.FileWriter;

public class EscreverArquivo {
    public static void main(String[] args) {
        try {
            // cria um objeto FileWriter apontando para o arquivo de destino
            FileWriter arquivo = new FileWriter("meuarquivo.txt");
            // escreve no arquivo utilizando o método write()
            arquivo.write("Este é um texto que será escrito no arquivo.");
            // fecha o arquivo
            arquivo.close();
            System.out.println("Arquivo salvo com sucesso!");
        } catch (IOException e) {
            System.out.println("Ocorreu um erro ao escrever o arquivo: " + e.getMessage());
        }
    }
}
```

Após a execução deste código, será gerado um arquivo chamado "meuarquivo.txt" com o texto "Este é um texto que será escrito no arquivo." dentro dele. É importante notar que, caso o arquivo já exista, o conteúdo anterior será sobrescrito.

## Mergulho profundo

A classe `FileWriter` possui diversos métodos que podem ser utilizados para escrever em um arquivo de texto. Além do `write()` utilizado no exemplo acima, também é possível utilizar o método `append()` para adicionar conteúdo ao final do arquivo. Outra opção é utilizar a classe `BufferedWriter` em conjunto com o `FileWriter`, que fornece métodos mais eficientes para escrita de arquivos.

É importante lembrar que, ao escrever em um arquivo de texto, é necessário definir o formato correto para salvar os caracteres. Por padrão, o Java utiliza o formato UTF-8, mas caso seja necessário, é possível alterar para outros formatos, como ANSI ou UTF-16. Para isso, basta utilizar o construtor do `FileWriter` que aceita um parâmetro do tipo `Charset` para definir o formato desejado.

## Veja também

- [Guia de Programação em Java: Leitura e Escrita de Arquivos](https://www.devmedia.com.br/guia/java-escrita-de-arquivos/39622)
- [Documentação da classe FileWriter do Java](https://docs.oracle.com/javase/7/docs/api/java/io/FileWriter.html)
- [Documentação da classe Charset do Java](https://docs.oracle.com/javase/7/docs/api/java/nio/charset/Charset.html)