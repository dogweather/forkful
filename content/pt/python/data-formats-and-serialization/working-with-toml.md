---
title:                "Trabalhando com TOML"
aliases:
- /pt/python/working-with-toml.md
date:                  2024-01-26T04:25:36.070986-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabalhando com TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/working-with-toml.md"
---

{{< edit_this_page >}}

## O Que & Por Que?
TOML, abreviatura para Tom's Obvious, Minimal Language (Linguagem Mínima e Óbvia do Tom), é um formato de serialização de dados semelhante ao JSON ou YAML, mas visa simplicidade e legibilidade. Programadores utilizam TOML para arquivos de configuração porque ele é fácil de escrever e entender, e mapeia de forma ordenada para estruturas de dados em linguagens de programação como Python.

## Como Fazer:
Antes de começar, instale o pacote `toml` com `pip install toml`. Vamos analisar um arquivo TOML:

```python
import toml

# Exemplo de conteúdo TOML como uma string
toml_string = """
[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z # Datas de primeira classe

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
"""

# Analisar a string TOML
parsed_toml = toml.loads(toml_string)

# Acessando dados
print(parsed_toml['owner']['name'])  # Saída: Tom Preston-Werner
print(parsed_toml['database']['ports'])  # Saída: [8001, 8001, 8002]
```

## Aprofundando-se
TOML foi criado por Tom Preston-Werner, um dos fundadores do GitHub, como um formato de arquivo de configuração mais amigável ao usuário. Ele é projetado para mapear de forma inequívoca para uma tabela hash e ser facilmente analisável por máquinas.

Comparado ao JSON, o TOML é mais legível para arquivos de configuração e suporta comentários. YAML, outra alternativa, pode ser mais compacto, mas a sua dependência de indentação e questões subtis, como a proibição de tabs, podem confundir as pessoas.

Quanto aos detalhes de implementação, os valores TOML são tipados, o que inclui strings, inteiros, floats, booleanos, datas, arrays e tabelas. Tudo é sensível a maiúsculas e minúsculas. Além disso, o TOML suporta strings multilinhas e, na versão mais recente, até permite arrays de tipos heterogêneos.

Python utiliza a biblioteca `toml`, que espelha as bibliotecas JSON e YAML em termos de API. Você tem `toml.load` e `toml.loads` para ler TOML de um arquivo ou uma string, respectivamente, e `toml.dump` e `toml.dumps` para escrevê-lo.

## Veja Também
- O repositório GitHub oficial do TOML para especificações: [github.com/toml-lang/toml](https://github.com/toml-lang/toml)
- A documentação da biblioteca `toml` Python: [pypi.org/project/toml/](https://pypi.org/project/toml/)
- Exemplos reais de TOML: Arquivos de configuração para o gerenciador de pacotes do Rust `cargo` ou a ferramenta de empacotamento Python `poetry`.
