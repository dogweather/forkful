---
title:                "Trabalhando com csv."
html_title:           "Fish Shell: Trabalhando com csv."
simple_title:         "Trabalhando com csv."
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/working-with-csv.md"
---

{{< edit_this_page >}}

## O que & Por quê?

Trabalhar com arquivos CSV (Comma Separated Values) é uma tarefa muito comum para programadores. Isso ocorre porque os CSVs são uma forma simples e eficiente de armazenar dados tabulares, ou seja, dados organizados em colunas e linhas, como uma planilha. É comum encontrar arquivos CSV sendo usados para armazenar dados de tabelas de banco de dados, resultados de pesquisas ou até mesmo informações de contatos.

## Como fazer:

Usar o Fish Shell para trabalhar com arquivos CSV é simples e prático. Primeiro, você precisará ler o arquivo CSV usando o comando `read` e especificando o separador desejado. Você pode usar o separador padrão "," ou especificar um diferente com a opção `-d`. Em seguida, você pode usar os comandos do Fish Shell, como o `for` loop, para manipular os dados do CSV da maneira que desejar.

- Leitura de um arquivo CSV:

```Fish Shell
read -d "," arquivo.csv
```

- Usando o `for` loop para percorrer as linhas do CSV:

```Fish Shell
for linha in $arquivo
    echo $linha
end
```

- Adicionando novas informações a um CSV existente:

```Fish Shell
echo "João, Silva" >> arquivo.csv
```

## Profundidade:

Arquivos CSV são originários dos primeiros dias da computação e foram criados para serem usados em programas que processavam dados em linhas de texto. Hoje em dia, existem opções mais avançadas para trabalhar com dados tabulares, como JSON e XML. No entanto, os CSVs ainda são amplamente usados devido à sua simplicidade e compatibilidade com muitas ferramentas e linguagens de programação.

Além de usar o Fish Shell, existem outras opções para trabalhar com arquivos CSV, como usar bibliotecas específicas em outras linguagens de programação, como Python ou R, ou utilizar ferramentas de visualização de dados, como o Microsoft Excel ou o Google Sheets.

O Fish Shell possui excelentes recursos para processar e manipular dados CSV, como a possibilidade de usar filtros e operações matemáticas diretamente no shell. Além disso, o Fish Shell possui uma documentação detalhada e uma comunidade ativa, o que facilita a resolução de problemas e o aprendizado.

## Veja também:

- Documentação oficial do Fish Shell: https://fishshell.com/docs/current/index.html
- Comandos do Fish Shell para trabalhar com CSV: https://fishshell.com/docs/current/commands.html#read
- Tutorial em vídeo sobre como trabalhar com CSV no Fish Shell: https://www.youtube.com/watch?v=5OX2qxpeWMk