---
title:                "Java: Trabalhando com arquivos csv"
simple_title:         "Trabalhando com arquivos csv"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/working-with-csv.md"
---

{{< edit_this_page >}}

## Por que trabalhar com arquivos CSV?

Trabalhar com arquivos CSV (Comma-Separated Values) é uma habilidade importante para quem lida com dados em qualquer área. CSV é um formato de arquivo simples e eficiente para armazenar dados em uma tabela, que pode ser facilmente lido e manipulado por programas e ferramentas de análise de dados.

## Como fazer

Para trabalhar com arquivos CSV em Java, é necessário utilizar a biblioteca java.io e suas classes FileReader e BufferedReader. É importante lembrar de tratar possíveis erros durante a leitura do arquivo. Veja um exemplo de código abaixo:

```
import java.io.File;
import java.io.FileReader;
import java.io.BufferedReader;

public class CSVReader {
    public static void main(String[] args) {
        File file = new File("dados.csv");
        try {
            FileReader reader = new FileReader(file);
            BufferedReader br = new BufferedReader(reader);
            String line;
            while ((line = br.readLine()) != null) {
                // faz algo com cada linha do arquivo
                System.out.println(line);
            }
            br.close();
        } catch (IOException e) {
            // tratamento de erros
            e.printStackTrace();
        }
    }
}
```

Ao executar o código acima, a saída será algo como:

```
nome,idade,cidade
João,30,São Paulo
Maria,25,Rio de Janeiro
Pedro,40,Belo Horizonte
```

## Aprofundando

Existem várias opções de bibliotecas e frameworks que podem facilitar a leitura e manipulação de arquivos CSV em Java, como o OpenCSV e o Apache Commons CSV. Além disso, é possível definir separadores e delimitadores personalizados em caso de necessidade.

É importante lembrar também de tratar possíveis problemas com caracteres especiais, como acentos, durante a leitura e escrita dos dados em um arquivo CSV.

## Veja também
- [Tutorial de leitura de CSV em Java com OpenCSV](https://www.baeldung.com/java-csv)
- [Documentação da classe FileReader](https://docs.oracle.com/javase/8/docs/api/java/io/FileReader.html)
- [Apache Commons CSV - Página oficial](https://commons.apache.org/proper/commons-csv/)
- [OpenCSV - Página oficial](http://opencsv.sourceforge.net/)