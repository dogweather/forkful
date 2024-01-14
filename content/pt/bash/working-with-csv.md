---
title:                "Bash: Trabalhando com arquivos csv"
simple_title:         "Trabalhando com arquivos csv"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/working-with-csv.md"
---

{{< edit_this_page >}}

## Por que trabalhar com CSV é útil?

CSV (Comma Separated Values, ou Valores Separados por Vírgula) é um formato de arquivo amplamente utilizado para armazenar dados de maneira tabular. É especialmente útil para armazenar grandes quantidades de informações, como listas de produtos, registros de clientes, dados financeiros, entre outros. Ao trabalhar com arquivos CSV, é possível organizar e manipular facilmente os dados para análise e relatórios.

## Como trabalhar com CSV

Existem várias maneiras de trabalhar com arquivos CSV em um script Bash. Vamos dar uma olhada em algumas das operações mais comuns:

### Lendo um arquivo CSV

Para ler um arquivo CSV em um script Bash, podemos usar o comando `read` combinado com o operador de redirecionamento `<` para ler um arquivo específico. Vamos supor que temos um arquivo chamado "produtos.csv" com a seguinte estrutura:

```
nome,preco,categoria
Celular,2000,eletronicos
Livro,50,literatura
```

Podemos utilizar o seguinte código para ler e imprimir os dados no arquivo:

```bash
#!/bin/bash
while IFS=',' read -r nome preco categoria
do
  echo "Nome do produto: $nome"
  echo "Preço: $preco"
  echo "Categoria: $categoria"
done < produtos.csv
```

Isso irá imprimir todos os produtos, seus preços e categorias na tela. O comando `IFS=','` indica que usaremos a vírgula como delimitador para separar as colunas do arquivo CSV.

### Escrevendo em um arquivo CSV

Também podemos usar o operador de redirecionamento `>` para escrever dados em um arquivo CSV. Por exemplo, se quisermos adicionar um novo produto ao arquivo "produtos.csv", podemos usar o seguinte código:

```bash
#!/bin/bash
nome="Teclado"
preco=100
categoria="informatica"
echo "$nome,$preco,$categoria" >> produtos.csv
```

Isso irá adicionar uma nova linha com o produto "Teclado" ao final do arquivo.

### Convertendo para CSV

Às vezes, precisamos converter outros formatos de arquivo em CSV. Para isso, podemos usar a ferramenta `csvtool`, que está disponível na maioria das distribuições do Linux.

Por exemplo, se tivermos um arquivo de texto com dados de clientes, podemos converter para CSV utilizando o seguinte comando:

```bash
csvtool -t '|' -u ',' cat clientes.txt > clientes.csv
```

Isso irá converter o arquivo usando o caractere `|` como delimitador e substituirá o arquivo original pelo arquivo CSV resultante.

## Mergulho Profundo

Trabalhar com arquivos CSV em scripts Bash pode se tornar ainda mais poderoso adicionando `awk` e outras ferramentas de processamento de texto. Além disso, é importante lembrar de sempre validar e limpar os dados antes de usá-los, especialmente se eles forem provenientes de uma fonte externa.

## Veja Também

- Documentação oficial do Bash: https://www.gnu.org/software/bash/manual/
- Documentação do `csvtool`: https://www.unix.com/man-page/redhat/1/csvtool/
- Tutorial de `awk`: https://www.gnu.org/software/gawk/manual/gawk.html