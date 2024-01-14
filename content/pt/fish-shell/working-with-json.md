---
title:                "Fish Shell: Trabalhando com json"
simple_title:         "Trabalhando com json"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/working-with-json.md"
---

{{< edit_this_page >}}

## Por que usar JSON na programação com o Fish Shell?

JSON, sigla para JavaScript Object Notation, é um formato de dados leve e versátil amplamente utilizado na comunicação entre sistemas e no armazenamento de informações. Ao trabalhar com o Fish Shell, o uso de JSON pode facilitar a manipulação e processamento de dados, tornando o processo de programação mais eficiente e eficaz.

## Como usar JSON no Fish Shell

Para utilizar JSON no Fish Shell, primeiro é necessário ter o pacote `jq` instalado. Esse pacote permite a manipulação de dados em JSON diretamente no terminal. Em seguida, basta seguir os seguintes passos:

1. Defina uma variável com uma estrutura de dados em JSON:

```Fish Shell
set dados '{"nome": "Maria", "idade": 30, "cidade": "São Paulo"}'
```

2. Utilize o comando `jq` para extrair informações específicas do JSON utilizando a sintaxe `.[chave]`:

```Fish Shell
echo $dados | jq '.nome'
```

Isso irá retornar apenas o valor da chave "nome", no caso, "Maria". Além disso, é possível utilizar o operador de atribuição `=` para armazenar esse valor em uma nova variável, como por exemplo:

```Fish Shell
set nome (echo $dados | jq '.nome')
```

3. O `jq` também permite filtrar e manipular dados mais complexos, como por exemplo:

```Fish Shell
echo $dados | jq '.idade | .+5'
```

Isso irá retornar o valor da idade (30) somado a 5, resultando em 35.

## Detalhes sobre o uso de JSON no Fish Shell

Além das funcionalidades mencionadas acima, é possível também utilizar loops e condicionais para trabalhar com dados em JSON no Fish Shell. Outro recurso interessante é a possibilidade de converter dados de JSON para CSV, tornando mais fácil a manipulação de grandes conjuntos de dados.

É importante ressaltar que, assim como em qualquer outro formato de dados, é necessário ter cuidado e garantir que o JSON esteja bem formatado e válido para evitar erros no processamento. Além disso, é recomendado estar familiarizado com a sintaxe de JSON para aproveitar ao máximo todas as suas funcionalidades no Fish Shell.

## Veja também

- <https://fishshell.com/>
- <https://stedolan.github.io/jq/>
- <https://www.json.org/>