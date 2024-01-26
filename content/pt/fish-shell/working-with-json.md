---
title:                "Trabalhando com JSON"
html_title:           "Arduino: Trabalhando com JSON"
simple_title:         "Trabalhando com JSON"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/working-with-json.md"
---

{{< edit_this_page >}}

## O Quê & Por Quê?

JSON é um formato de troca de dados leve e fácil de ler para humanos, muito usado para armazenar e transportar informações. Programadores o utilizam amplamente devido à sua simplicidade e interoperabilidade com diferentes linguagens de programação.

## Como Fazer:

Para trabalhar com JSON no Fish Shell, podemos usar `jq`, uma ferramenta de linha de comando para processar JSON. Aqui estão alguns exemplos básicos:

```Fish Shell
# Instale o jq
sudo apt-get install jq

# Formatando JSON com jq
echo '{"nome": "João", "idade": 25}' | jq '.'

# Acessando valores específicos
echo '{"nome": "João", "idade": 25}' | jq '.nome'

# Acessando arrays
echo '{"nomes": ["João", "Maria", "José"]}' | jq '.nomes[1]'
```

Saída esperada:
```
{
  "nome": "João",
  "idade": 25
}
"João"
"Maria"
```

## Mergulho Profundo

O JSON surgiu em 2001, proposto por Douglas Crockford, como uma alternativa ao XML. Hoje, ferramentas como `jq`, `jshon` e funções integradas em linguagens de programação são usadas para trabalhar com JSON. Além disso, bancos de dados NoSQL, como MongoDB, usam o JSON como formato para armazenamento de dados. No Fish Shell, especificamente, a utilização de utilitários externos é necessária uma vez que não há uma funcionalidade interna exclusiva para o processamento de JSON.

## Veja Também

- Documentação oficial do `jq`: https://stedolan.github.io/jq/manual/
- Tutorial de JSON: https://www.w3schools.com/js/js_json_intro.asp
- Breve história do JSON: https://www.json.org/json-pt.html
- Sobre o Fish Shell: https://fishshell.com/
