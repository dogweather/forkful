---
title:                "Trabalhando com arquivos csv"
html_title:           "Ruby: Trabalhando com arquivos csv"
simple_title:         "Trabalhando com arquivos csv"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/working-with-csv.md"
---

{{< edit_this_page >}}

## Por que

Se você é um desenvolvedor iniciante ou experiente, provavelmente já deve ter se deparado com arquivos CSV em algum momento da sua carreira. Eles são amplamente utilizados para armazenar dados tabulares de uma forma simples e fácil de entender. Neste artigo, vamos explorar como trabalhar com arquivos CSV em Ruby e como isso pode facilitar seu trabalho com dados.

## Como Fazer

Para começar, vamos precisar da biblioteca CSV nativa do Ruby. Ela já vem instalada com a linguagem, então não precisamos nos preocupar em instalar nada adicional. Vamos supor que temos um arquivo CSV chamado "contatos.csv" contendo informações de contato de várias pessoas em um formato de tabela, com as colunas "Nome", "Sobrenome" e "Email". Vamos ver como podemos ler e manipular esses dados em nosso código Ruby.

Primeiro, precisamos requerir a biblioteca CSV no início do nosso arquivo:

```Ruby
require 'csv'
```

Agora podemos usar o método `foreach` da biblioteca CSV para iterar sobre as linhas do arquivo e transformá-las em arrays contendo os valores de cada coluna:

```Ruby
CSV.foreach('contatos.csv') do |row|
  puts row.inspect
end
```

Rodando esse código, teremos como resultado:

```Ruby
["Nome", "Sobrenome", "Email"]
["João", "Silva", "joao.silva@gmail.com"]
["Maria", "Souza", "maria.souza@gmail.com"]
["Pedro", "Ferreira", "pedro.ferreira@gmail.com"]
```

Agora que temos os dados do arquivo em arrays, podemos manipulá-los da forma que desejarmos, como por exemplo, criar uma lista de emails para enviar uma newsletter. Selecionando apenas a coluna "Email" do nosso arquivo, podemos criar essa lista assim:

```Ruby
emails = []

CSV.foreach('contatos.csv') do |row|
  emails << row[2] # índice 2 é a coluna "Email"
end

puts emails
```

Rodando esse código, teremos como resultado:

```
joao.silva@gmail.com
maria.souza@gmail.com
pedro.ferreira@gmail.com
```

Você também pode fazer o caminho contrário, e criar um arquivo CSV a partir de dados em arrays. Veja esse exemplo:

```Ruby
require 'csv'

lista = [
  ["Chave", "Valor"],
  ["Nome", "John"],
  ["Sobrenome", "Smith"],
  ["Email", "john.smith@gmail.com"]
]

CSV.open('dados.csv', 'wb') do |csv|
  lista.each do |linha|
    csv << linha
  end
end
```

Esse código criará um novo arquivo chamado "dados.csv" contendo os seguintes dados:

```
Chave,Valor
Nome,John
Sobrenome,Smith
Email,john.smith@gmail.com
```

Com esses exemplos, você já tem uma base sólida para começar a trabalhar com arquivos CSV em Ruby. Mas se quiser se aprofundar ainda mais, continue lendo para descobrir algumas funcionalidades adicionais e dicas úteis.

## Deep Dive

Além dos métodos utilizados no exemplo anterior, a biblioteca CSV possui uma grande variedade de métodos úteis para trabalhar com arquivos CSV. Aqui estão alguns dos mais comuns:

- `CSV.read`: lê o arquivo e retorna uma matriz contendo todas as linhas e colunas do arquivo.
- `CSV.parse`: lê o arquivo e retorna uma matriz de matrizes representando cada linha e coluna. Útil quando você sabe que o CSV é um arquivo CSV válido e bem formado.
- `CSV.generate`: gera um arquivo CSV a partir de dados em arrays.
- `CSV.open`: abre o arquivo CSV especificado e permite realizar operações de leitura e escrita em seu conteúdo.
- `each`: itera sobre as linhas do arquivo e executa algum código especificado dentro do bloco.
- `foreach`: itera sobre as linhas do arquivo como `each`, mas já transforma cada linha em um array de strings.
- `<<`: adiciona uma nova linha ao arquivo CSV.

Uma funcionalidade interessante da biblioteca CSV é que ela nos permite especificar o delimitador e a codificação de caracteres do arquivo que estamos lendo ou escrevendo. Por padrão, o delimitador é