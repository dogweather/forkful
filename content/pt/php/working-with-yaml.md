---
title:                "PHP: Trabalhando com YAML"
simple_title:         "Trabalhando com YAML"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/working-with-yaml.md"
---

{{< edit_this_page >}}

# Por que trabalhar com YAML em programação PHP?

YAML (YAML Ain't Markup Language) é uma linguagem de formatação de dados de fácil leitura e escrita, utilizada em muitas aplicações de programação, incluindo PHP. Trabalhar com YAML pode facilitar o processo de armazenamento, transporte e leitura de dados estruturados em seu código.

## Como trabalhar com YAML em PHP

Para começar a trabalhar com YAML em PHP, você precisará da extensão YAML instalada em seu servidor. Você pode verificar se a extensão está habilitada verificando a saída da função `phpinfo()`. Se a extensão não estiver habilitada, você precisará habilitá-la no seu arquivo `php.ini`.

Uma vez que a extensão estiver habilitada, você pode começar a trabalhar com YAML em seu código PHP. Existem duas principais funções que você pode usar: `yaml_parse()` e `yaml_emit()`. A função `yaml_parse()` converte um arquivo YAML em uma matriz PHP, enquanto `yaml_emit()` converte uma matriz PHP em um arquivo YAML.

Aqui está um exemplo simples de como usar essas funções:

```PHP
// Carregar o conteúdo de um arquivo YAML em uma variável
$conteudo = file_get_contents('dados.yaml');

// Converter o conteúdo em uma matriz PHP
$dados = yaml_parse($conteudo);

// Acessar os dados da matriz
echo "Nome: " . $dados['nome'] . "\n";
echo "Idade: " . $dados['idade'] . "\n";
echo "Cidade: " . $dados['cidade'] . "\n";

// Criar uma nova matriz com os dados
$novo_dados = [
    'nome' => 'Maria',
    'idade' => 30,
    'cidade' => 'São Paulo'
];

// Converter a matriz em um arquivo YAML
$novo_conteudo = yaml_emit($novo_dados);

// Escrever o conteúdo em um novo arquivo YAML
file_put_contents('novos_dados.yaml', $novo_conteudo);
```

O código acima irá ler um arquivo YAML com informações de uma pessoa e exibir essas informações na tela. Em seguida, ele irá criar uma nova matriz com dados diferentes e gravar esses dados em um novo arquivo YAML.

## Aprofundando-se em YAML

Embora YAML seja uma linguagem simples e fácil de usar, ele também possui muitos recursos avançados que podem ser úteis em projetos de programação mais complexos. Alguns desses recursos incluem:

- Aninhamento de dados: você pode usar a sintaxe de recuo em YAML para aninhar dados e criar estruturas mais complexas.
- Referências: você pode usar referências para apontar para dados existentes em outras partes do arquivo YAML.
- Comentários: você pode adicionar comentários em seu arquivo YAML para documentar seu código.

Para aprender mais sobre esses e outros recursos avançados, você pode consultar a documentação oficial do YAML.

# Veja também

- [Documentação oficial do YAML](https://yaml.org/)
- [Extensão YAML do PHP](https://www.php.net/manual/en/book.yaml.php)