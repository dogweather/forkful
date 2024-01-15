---
title:                "Trabalhando com yaml"
html_title:           "TypeScript: Trabalhando com yaml"
simple_title:         "Trabalhando com yaml"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/working-with-yaml.md"
---

{{< edit_this_page >}}

## Por que trabalhar com YAML? 

Trabalhar com YAML pode facilitar a leitura e escrita de dados de configuração em projetos de desenvolvimento. Além disso, com a utilização de TypeScript, é possível garantir uma maior validação e segurança desses dados.

## Como fazer:

Para começar a trabalhar com YAML em um projeto TypeScript, você precisará das seguintes ferramentas:

- **npm** para gerenciar as dependências do projeto.
- **yamljs** para fazer a leitura e escrita de dados em formato YAML.
- **@types/yamljs** para adicionar suporte ao TypeScript para a biblioteca yamljs.

Primeiramente, crie um novo projeto TypeScript utilizando o comando `tsc --init` no terminal. Em seguida, instale as dependências necessárias utilizando o comando `npm install yamljs @types/yamljs`. Agora, você está pronto para começar a trabalhar com YAML no seu projeto.

### Exemplos de código: 

*Leitura de dados YAML:*

```TypeScript
import * as yaml from 'yamljs';

// Lê um arquivo YAML e armazena os dados em uma variável
const config = yaml.load('./config.yaml');

// Acessa os dados do arquivo YAML
console.log(config.apiKey);
console.log(config.baseUrl);
```

*Escrita de dados YAML:*

```TypeScript
import * as yaml from 'yamljs';

// Cria um objeto com os dados a serem escritos
const data = {
    name: 'John',
    age: 27,
    hobbies: ['coding', 'reading', 'hiking']
};

// Converte o objeto em formato YAML e o escreve em um arquivo
yaml.dump(data, 'output.yaml');
```

O código acima resulta em um arquivo YAML com a seguinte estrutura:

```yaml
name: John
age: 27
hobbies:
  - coding
  - reading
  - hiking
```

## Aprofundando:

Além das funções básicas de leitura e escrita de dados YAML, é possível realizar outras operações utilizando a biblioteca yamljs. Aqui estão algumas delas:

- **Validação de dados:** utilizando a função `yaml.validate()`, é possível verificar se um objeto possui a estrutura esperada de acordo com um esquema pré-definido.
- **Mapeamento de dados:** com a função `yaml.loadAll()`, é possível carregar múltiplos documentos YAML em um único objeto.
- **Manipulação de dados:** utilizando a função `yaml.set()`, é possível alterar valores em um objeto carregado a partir de um arquivo YAML.

Para mais informações e exemplos, consulte a documentação oficial da biblioteca yamljs em [https://github.com/jeremyfa/yamljs](https://github.com/jeremyfa/yamljs).

## Veja também:

- [Documentação oficial do TypeScript](https://www.typescriptlang.org/docs/)
- [Tutorial de YAML para iniciantes](https://gettaurus.org/docs/YAMLTutorial/#structure)
- [Exemplos práticos com YAML e TypeScript](https://medium.com/@cdeniz/typescript-with-yaml-multi-environments-part-2-49b9c8b0aafc)

Fique à vontade para explorar e utilizar o YAML em seus projetos de desenvolvimento com TypeScript. Tenha em mente que a prática leva à perfeição, então não desista se encontrar algum desafio durante o processo. Com o tempo, você irá se tornar um expert em trabalhar com YAML e aproveitará todos os benefícios que essa combinação pode oferecer.