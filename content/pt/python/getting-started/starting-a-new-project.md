---
changelog:
- 2024-02-22, dogweather, reviewed
- 2024-02-22, gpt-4-0125-preview, translated from English
date: 2024-02-22 17:30:12.949979-07:00
description: "Iniciar um novo projeto em Python \xE9 sobre configurar uma estrutura\
  \ organizada e sustent\xE1vel desde o in\xEDcio. Os programadores fazem isso para\
  \ garantir que\u2026"
lastmod: '2024-03-13T22:44:46.154552-06:00'
model: gpt-4-0125-preview
summary: "Iniciar um novo projeto em Python \xE9 sobre configurar uma estrutura organizada\
  \ e sustent\xE1vel desde o in\xEDcio. Os programadores fazem isso para garantir\
  \ que\u2026"
title: Iniciando um novo projeto
---

{{< edit_this_page >}}

## O Que & Por Quê?

Iniciar um novo projeto em Python é sobre configurar uma estrutura organizada e sustentável desde o início. Os programadores fazem isso para garantir que seu código seja fácil de ler, depurar e colaborar, especialmente à medida que o projeto e a equipe que trabalha nele crescem com o tempo.

## Como fazer:

### Criar um Ambiente Virtual
Um ambiente virtual é um diretório autocontido que contém todos os executáveis necessários para usar os pacotes que um projeto Python precisaria. É aconselhável criar um ambiente virtual para cada projeto para evitar conflitos entre dependências do projeto. Use o módulo `venv`, que faz parte da biblioteca padrão do Python.

```shell
# Substitua 'meuprojeto' pelo nome do seu projeto
python3 -m venv meuprojeto-env
```

Para ativar o ambiente virtual:

No Windows:
```shell
meuprojeto-env\Scripts\activate.bat
```

No Unix ou MacOS:
```shell
source meuprojeto-env/bin/activate
```

Saída de Exemplo (a saída pode variar ligeiramente dependendo do SO):
```shell
(meuprojeto-env) $
```

### Instalando Pacotes
Use `pip`, o instalador de pacotes para Python, para instalar, atualizar e remover pacotes. Aqui está como você pode instalar uma biblioteca de terceiros popular, `requests`, para fazer solicitações HTTP:

```shell
pip install requests
```

Saída de Exemplo:
```shell
Collecting requests
  Downloading requests-2.25.1-py2.py3-none-any.whl (61 kB)
     |████████████████████████████████| 61 kB 1.3 MB/s
Installing collected packages: requests
Successfully installed requests-2.25.1
```

### Configurando uma Estrutura de Projeto
Um projeto Python típico pode parecer algo assim:

```
meuprojeto/
│
├── meuprojeto-env/    # Ambiente virtual
├── docs/              # Documentação
├── tests/             # Testes unitários e de integração
│   └── __init__.py
├── meuprojeto/        # Código fonte do projeto
│   ├── __init__.py
│   └── main.py
├── setup.py           # Arquivo de configuração do projeto
└── README.md          # Visão geral do projeto
```

### Crie Seu Primeiro Programa
Crie um arquivo `main.py` dentro do diretório `meuprojeto`. Aqui está um exemplo de um programa simples:

```python
# meuprojeto/meuprojeto/main.py
def greet(name):
    return f"Olá, {name}!"

if __name__ == "__main__":
    print(greet("Mundo"))
```

Execute seu programa:

```shell
python meuprojeto/main.py
```

Saída de Exemplo:
```shell
Olá, Mundo!
```

### Use um Framework para Projetos Maiores
Para projetos maiores, especialmente aplicações web, frameworks como Django ou Flask são inestimáveis. Aqui está como instalar o Flask e criar uma simples aplicação web "Hello, World":

```shell
pip install Flask
```

Crie um arquivo `app.py` com o seguinte conteúdo:

```python
# app.py
from flask import Flask
app = Flask(__name__)

@app.route("/")
def hello_world():
    return "<p>Olá, Mundo!</p>"

if __name__ == "__main__":
    app.run(debug=True)
```

Execute a aplicação Flask:

```shell
flask run
```

Saída de Exemplo:
```shell
 * Running on http://127.0.0.1:5000/ (Pressione CTRL+C para sair)
```

Navegue até `http://127.0.0.1:5000/` no seu navegador web, e você deverá ver a mensagem "Olá, Mundo!".
