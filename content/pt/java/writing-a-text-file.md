---
title:                "Java: Escrevendo um arquivo de texto"
simple_title:         "Escrevendo um arquivo de texto"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por que

Escrever um arquivo de texto pode parecer uma tarefa simples e básica, mas é uma habilidade essencial para programadores Java. Através da escrita de arquivos de texto, é possível armazenar e manipular dados importantes, como configurações de aplicativos, registros de atividades e muito mais. Além disso, a escrita de arquivos pode ser útil para gerar relatórios ou exportar dados para outros programas.

## Como fazer

Aqui está um exemplo de como escrever um arquivo de texto em Java:

```Java
import java.io.*;

public class EscrevendoArquivo {
  public static void main(String[] args) {
    try {
      // cria um objeto para escrever no arquivo
      FileWriter fw = new FileWriter("meu_arquivo.txt");
      // escreve uma string no arquivo
      fw.write("Este é meu primeiro arquivo de texto!");
      // fecha o objeto FileWriter
      fw.close();
      // exibe uma mensagem de sucesso
      System.out.println("Arquivo de texto criado com sucesso!");
      
      // lê o arquivo criado
      FileReader fr = new FileReader("meu_arquivo.txt");
      // cria um objeto BufferedReader para ler linhas do arquivo
      BufferedReader br = new BufferedReader(fr);
      // variável para armazenar cada linha lida do arquivo
      String linha = "";
      // loop para ler e exibir todas as linhas do arquivo
      while ((linha = br.readLine()) != null) {
        System.out.println(linha);
      }
      // fecha o objeto BufferedReader
      br.close();
    } catch (IOException e) {
      // caso ocorra um erro ao escrever ou ler o arquivo
      e.printStackTrace();
    }
  }
}
```

A saída deste código será um arquivo de texto chamado "meu_arquivo.txt" com o conteúdo "Este é meu primeiro arquivo de texto!" e a frase será exibida no console.

## Aprofundando

Ao escrever um arquivo de texto em Java, é importante lembrar que cada caractere é armazenado em sua representação ASCII. Ou seja, caracteres especiais, como acentos e símbolos, podem não ser exibidos corretamente em outras aplicações.

Além disso, é fundamental fechar corretamente o objeto FileWriter após escrever no arquivo, pois isso garante que os dados sejam armazenados de forma correta e que o recurso de gravação seja liberado para outros processos.

Outra dica importante é utilizar a classe BufferedWriter ao invés do FileWriter, pois isso pode melhorar a performance ao escrever grandes quantidades de dados em arquivos.

## Veja também

- [Documentação oficial do Java sobre escrita de arquivos](https://docs.oracle.com/javase/8/docs/api/java/io/FileWriter.html)
- [Tutorial sobre leitura e escrita de arquivos em Java](https://www.tutorialspoint.com/java/java_files_io.htm)
- [Guia completo para leitura e escrita de arquivos em Java](https://www.baeldung.com/java-write-to-file)