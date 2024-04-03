---
date: 2024-01-26 04:25:36.070986-07:00
description: "TOML, abreviatura para Tom's Obvious, Minimal Language (Linguagem M\xED\
  nima e \xD3bvia do Tom), \xE9 um formato de serializa\xE7\xE3o de dados semelhante\
  \ ao JSON ou\u2026"
lastmod: '2024-03-13T22:44:46.178631-06:00'
model: gpt-4-0125-preview
summary: "TOML, abreviatura para Tom's Obvious, Minimal Language (Linguagem M\xED\
  nima e \xD3bvia do Tom), \xE9 um formato de serializa\xE7\xE3o de dados semelhante\
  \ ao JSON ou YAML, mas visa simplicidade e legibilidade."
title: Trabalhando com TOML
weight: 39
---

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
