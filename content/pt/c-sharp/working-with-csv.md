---
title:                "C#: Trabalhando com arquivos csv."
simple_title:         "Trabalhando com arquivos csv."
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/working-with-csv.md"
---

{{< edit_this_page >}}

## Por que trabalhar com CSV?

CSV (Comma Separated Values) é um formato de arquivo amplamente utilizado para armazenar dados tabulares em um formato legível por máquina, tornando-o uma escolha popular para armazenar grandes quantidades de dados. Trabalhar com CSV no C# pode facilitar a leitura e gravação de dados, bem como simplificar o processo de análise desses dados. 

## Como fazer:

Se você está interessado em trabalhar com CSV no C#, é importante primeiro ter conhecimento básico da linguagem de programação. Em seguida, você precisará de uma ideia ou projeto que envolva a leitura e gravação de dados em formato CSV. Vamos ver um exemplo simples de como ler e gravar dados em CSV no C#:

```C#
//Lendo dados de um arquivo CSV
using (var reader = new StreamReader("dados.csv"))
{
    var line = "";
    while ((line = reader.ReadLine()) != null)
    {
        var values = line.Split(','); //separa os valores em cada linha pelo caracter ","
        var nome = values[0]; //primeiro valor é o nome
        var idade = int.Parse(values[1]); //segundo valor é a idade (convertido para inteiro)
        var salario = Decimal.Parse(values[2]); //terceiro valor é o salário (convertido para decimal)

        //faça o que quiser com os dados lidos, como exibir na tela:
        Console.WriteLine($"Nome: {nome}");
        Console.WriteLine($"Idade: {idade}");
        Console.WriteLine($"Salário: {salario} \n");
    }
}

//Gravando dados em um arquivo CSV
using (var writer = new StreamWriter("dados.csv"))
{
    writer.WriteLine("Maria, 32, 5000.00"); //escreve uma linha com os dados separados por ","
    writer.WriteLine("João, 25, 3000.00");
    //adiciona os dados que desejar
}
```

Com este código, é possível ler dados de um arquivo CSV já existente e também gravar novos dados nesse mesmo arquivo.

## Deep Dive:

Existem várias bibliotecas disponíveis em C# para ajudar na leitura e gravação de dados em formato CSV, como o "CsvHelper" e o "FileHelpers". Essas bibliotecas possuem recursos avançados, como mapeamento de objetos, o que pode tornar o processo de leitura e gravação de dados ainda mais simples. Outra dica importante é sempre ter cuidado com a formatação dos dados em um arquivo CSV, certificando-se de que eles estejam de acordo com o padrão UTF-8.

## Veja também
- Documentação oficial do C# sobre leitura e gravação de dados em formato CSV: [https://docs.microsoft.com/pt-br/dotnet/api/system.io.streamreader?view=net-5.0](https://docs.microsoft.com/pt-br/dotnet/api/system.io.streamreader?view=net-5.0)
- CsvHelper: [https://joshclose.github.io/CsvHelper/](https://joshclose.github.io/CsvHelper/)
- FileHelpers: [https://www.filehelpers.net/](https://www.filehelpers.net/)