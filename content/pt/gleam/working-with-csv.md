---
title:                "Gleam: Trabalhando com csv."
simple_title:         "Trabalhando com csv."
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/working-with-csv.md"
---

{{< edit_this_page >}}

## Por que trabalhar com CSV?

O formato CSV (Comma-Separated Values) é amplamente utilizado para armazenar e compartilhar dados tabulares. Ele é simples, legível por humanos e pode ser aberto em praticamente qualquer programa de planilha eletrônica. Trabalhar com arquivos CSV é uma habilidade valiosa para profissionais que lidam com dados e também pode ser útil para fins pessoais, como organizar informações em uma planilha.

## Como trabalhar com CSV em Gleam

Trabalhar com arquivos CSV em Gleam é simples e direto. Comece importando o módulo `csv`:

```
import csv
```

Em seguida, abra o arquivo CSV desejado usando a função `csv.open()` e especifique o modo de leitura (por exemplo, `csv.read` para ler o arquivo):

```
file = csv.open("arquivo.csv", csv.Read)
```

Agora você pode acessar os dados do arquivo usando a variável `file`. Por exemplo, para imprimir todas as linhas do arquivo:

```
for row in file {
  io.println(row)
}
```

## Mergulho profundo em trabalhar com CSV

Ao trabalhar com arquivos CSV em Gleam, é importante lembrar que ele lida com tipos fortemente definidos, o que significa que suas colunas devem ser consistentes e precisam ser convertidas em tipos Gleam apropriados (por exemplo, `Int` para números inteiros ou `String` para valores de texto).

Você também pode separar e analisar linhas individuais usando a função `csv.parse()`:

```
line = "nome, sobrenome, idade"
parsed = csv.parse(line)
// parsed = ["nome", "sobrenome", "idade"]
```

Além disso, o módulo `csv` oferece suporte para gravar em arquivos CSV com a função `csv.write()` e também permite especificar um delimitador personalizado em vez da vírgula padrão.

## Veja também

- Documentação oficial do módulo `csv`: https://gleam.run/modules/csv/
- Tutorial de CSV em Gleam: https://blog.gleam.run/csv-in-gleam.html
- Exemplo completo de manipulação de CSV em Gleam: https://gist.github.com/gdejohn/0023a3967e3062799b59e8584fa0c36a