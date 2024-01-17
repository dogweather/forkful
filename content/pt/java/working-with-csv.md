---
title:                "Trabalhando com csv"
html_title:           "Java: Trabalhando com csv"
simple_title:         "Trabalhando com csv"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/working-with-csv.md"
---

{{< edit_this_page >}}

## O que é CSV e por que os programadores o utilizam?
Trabalhar com CSV (Valores Separados por Vírgulas) é uma maneira de manipular dados em formato de tabela utilizando texto simples. Isso permite que os programadores possam facilmente importar e exportar dados de planilhas e bancos de dados. Além disso, o formato CSV é amplamente suportado por diferentes linguagens de programação e aplicações, tornando-o uma escolha popular entre os programadores.

## Como fazer:
Uma das maneiras mais simples de trabalhar com CSV em Java é utilizando a biblioteca OpenCSV. Aqui está um exemplo básico de como ler e imprimir dados de um arquivo CSV:

```
import com.opencsv.CSVReader;

public class LeitorCSV {
    public static void main(String[] args) {
        try {
            CSVReader reader = new CSVReader(new FileReader("dados.csv"));
            String[] linha;
            while ((linha = reader.readNext()) != null) {
                for (String coluna : linha) {
                    System.out.print(coluna + " ");
                }
                System.out.print("\n");
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```
Saída:
```
Nome Sobrenome Idade
João Silva 25
Maria Santos 30
Carlos Silva 20
```

## Profundidade:
O formato CSV foi criado na década de 1970 como uma maneira simples de armazenar dados em planilhas eletrônicas. Atualmente, existem outras alternativas de formato de arquivo para armazenamento de dados estruturados, como JSON e XML. No entanto, o CSV ainda é amplamente utilizado por sua simplicidade e facilidade de uso. Além disso, existem outras bibliotecas Java disponíveis, além do OpenCSV, que permitem trabalhar com CSV.

## Veja também:
- [OpenCSV docs] (http://opencsv.sourceforge.net/)
- [Tutorial: Lendo e escrevendo arquivos CSV em Java] (https://www.baeldung.com/java-csv)
- [Diferentes opções de bibliotecas para trabalhar com CSV em Java] (https://stackoverflow.com/questions/101100/csv-api-for-java)