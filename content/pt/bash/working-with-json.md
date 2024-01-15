---
title:                "Trabalhando com json"
html_title:           "Bash: Trabalhando com json"
simple_title:         "Trabalhando com json"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/working-with-json.md"
---

{{< edit_this_page >}}

## Por que

Se você trabalha com programação ou análise de dados, provavelmente já se deparou com arquivos JSON. JSON é uma forma comum de armazenar e transmitir dados estruturados, e saber como lidar com ele pode ser fundamental para o sucesso em diversas áreas de atuação.

## Como fazer

Para começar a trabalhar com JSON em Bash, você precisa ter em mente que tudo se baseia em manipulação de strings. O Bash não possui funções nativas para lidar com esse formato de dados, então é necessário utilizar ferramentas externas. Uma das opções mais populares é o utilitário "jq", que permite analisar e manipular dados JSON de forma eficiente.

Para instalar o jq, basta utilizar o gerenciador de pacotes da sua distribuição Linux. Por exemplo, no Ubuntu, você pode executar o seguinte comando:

```Bash
sudo apt-get install jq
```

Agora, vamos ver alguns exemplos práticos de como utilizar o jq para trabalhar com JSON:

### Exemplo 1: Listando valores específicos

Suponha que você tenha um arquivo JSON com informações de usuários, como nome, idade e e-mail. Para listar somente os nomes desses usuários, você pode usar o comando:

```Bash
jq '.nome' usuarios.json
```

Isso irá imprimir na tela somente os valores correspondentes à chave "nome" de cada objeto JSON no arquivo.

### Exemplo 2: Filtrando valores

É possível também realizar filtros mais específicos utilizando o jq. Por exemplo, para listar apenas os usuários que tenham mais de 18 anos, podemos usar o comando:

```Bash
jq '.[] | select(.idade > 18)' usuarios.json
```

Nesse caso, o filtro "select" é aplicado a cada objeto JSON presente no arquivo, e somente os que atendem a condição especificada serão apresentados na saída.

## Mergulho Profundo

O jq possui uma série de recursos avançados que permitem uma manipulação mais complexa de dados JSON. Além dos exemplos apresentados, você pode utilizar funções para realizar cálculos, agrupar informações e até mesmo criar novos objetos JSON. Para explorar mais sobre o jq, consulte a documentação oficial.

## Veja também

- [Documentação oficial do jq](https://stedolan.github.io/jq)
- [Artigo sobre o uso de JSON em Bash](https://medium.com/@fidelissauro/utilizando-json-em-bash-dbf74275b782)
- [Videoaula sobre o jq](https://www.youtube.com/watch?v=SM9VappyMHI)