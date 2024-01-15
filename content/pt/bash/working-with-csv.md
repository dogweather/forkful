---
title:                "Trabalhando com csv"
html_title:           "Bash: Trabalhando com csv"
simple_title:         "Trabalhando com csv"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/working-with-csv.md"
---

{{< edit_this_page >}}

## Por que usar CSV no Bash

CSV (Comma Separated Values) é um formato amplamente utilizado para armazenar dados estruturados em forma de tabelas, como planilhas. No Bash, trabalhar com arquivos CSV pode facilitar a manipulação e análise de dados, tornando seu código mais eficiente.

## Como fazer

Para trabalhar com arquivos CSV no Bash, primeiro é necessário ter um arquivo CSV disponível. Você pode criar um usando um editor de texto simples, como o Notepad ou o Nano. Certifique-se de separar cada coluna com uma vírgula e cada linha com uma quebra de linha.

### Lendo um arquivo CSV

Para ler um arquivo CSV no Bash, podemos usar o comando `read` combinado com o redirecionamento de entrada (`<`). Por exemplo, se tivermos um arquivo **dados.csv** com as colunas "Nome" e "Idade", podemos ler os dados da seguinte maneira:

```Bash
while IFS=, read nome idade
do
  echo "Nome: $nome, Idade: $idade"
done < dados.csv
```
Aqui, usamos `IFS=,` para especificar a vírgula como separador de campos ao invés do espaço padrão, e `read nome idade` para armazenar os valores de cada coluna nas variáveis `nome` e `idade`. Dentro do loop `do`, imprimimos esses valores com o comando `echo`.

### Escrevendo em um arquivo CSV

Para escrever em um arquivo CSV, primeiro precisamos redirecionar a saída para o arquivo desejado. Por exemplo, se quisermos adicionar uma nova linha ao nosso arquivo **dados.csv**, podemos fazer o seguinte:

```Bash
echo "Maria, 25" >> dados.csv
```
Isso irá adicionar uma nova linha com os valores "Maria" e "25" ao final do arquivo.

## Explorando mais recursos do Bash com CSV

Agora que já sabemos como ler e escrever em arquivos CSV no Bash, podemos aproveitar outras opções e comandos para manipular os dados. Por exemplo, podemos usar o operador `>>, <<, >|` para redirecionar a saída para um arquivo CSV com um cabeçalho, ou podemos usar o comando `cut` para selecionar apenas algumas colunas específicas do arquivo. Com um pouco de criatividade, é possível realizar várias tarefas com dados CSV no Bash.

## Veja também

- [Documentação oficial do Bash](https://www.gnu.org/software/bash/)
- [Tutorial sobre CSV no Bash](https://www.digitalocean.com/community/tutorials/how-to-handle-csv-in-bash)
- [Mais dicas e truques para trabalhar com CSV no Bash](https://www.cyberciti.biz/faq/unix-linux-bash-read-comma-separated-cvsfile/)