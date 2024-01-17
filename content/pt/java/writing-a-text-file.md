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

## O que e por que?

Escrever um arquivo de texto em Java é o processo de criar um arquivo contendo texto legível por humanos utilizando a linguagem de programação Java. Programadores fazem isso para armazenar informações, como dados de configuração, valores de variáveis ou resultados de saída, de forma persistente em um arquivo que pode ser acessado posteriormente.

## Como fazer:

Para escrever um arquivo de texto em Java, é preciso seguir alguns passos simples:

1. Importe a classe `java.io.FileWriter` no início do seu código.
2. Crie uma instância dessa classe, passando o caminho do arquivo e o argumento `true` para indicar que o arquivo será modificado.
3. Utilize o método `write()` da classe para escrever o conteúdo desejado no arquivo.
4. Não esqueça de fechar o objeto `FileWriter` com o método `close()` após terminar de escrever.

Veja um exemplo em código:

```Java
import java.io.FileWriter;

public class EscrevendoArquivo {

    public static void main(String[] args) {

        try {
            // Criando instância do FileWriter com o caminho do arquivo
            FileWriter arquivo = new FileWriter("arquivo.txt", true);

            // Escrevendo no arquivo
            arquivo.write("Olá, mundo!");

            // Fechando o arquivo
            arquivo.close();
        } catch (Exception e) {
            System.out.println("Ocorreu um erro ao escrever o arquivo: " + e.getMessage());
        }
    }
}
```

O resultado desse código será um arquivo chamado "arquivo.txt", localizado no mesmo diretório do código, contendo o texto "Olá, mundo!".

## Profundidade

Escrever arquivos de texto em Java é uma tarefa comum e essencial em muitos programas. Antigamente, essa tarefa era mais complicada e requerida mais código para ser realizada, mas atualmente, com o avanço da linguagem e das bibliotecas, tornou-se uma tarefa mais simples.

Existem outras formas de armazenar informações de forma persistente em Java, como por exemplo, utilizando bancos de dados. Porém, escrever arquivos de texto ainda é utilizado para necessidades mais simples e rápidas, além de ser uma forma mais acessível para iniciantes na programação.

## Veja também:

- [Documentação oficial do FileWriter em Java](https://docs.oracle.com/javase/7/docs/api/java/io/FileWriter.html)
- [Tutorial sobre escrita de arquivos em Java](https://www.devmedia.com.br/trabalhando-com-arquivos-em-java/23142)
- [Outras formas de armazenamento persistente em Java](https://www.caelum.com.br/apostila-java-orientacao-objetos/bancos-de-dados-e-jdbc/#16-7-mas-e-se-nao-quiser-usar-jdbc)