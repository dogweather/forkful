---
title:                "Baixando uma página da web"
html_title:           "Python: Baixando uma página da web"
simple_title:         "Baixando uma página da web"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## O que é e por quê?

Fazer o download de uma página da web significa obter o conteúdo de uma página da internet para uso em um programa. Os programadores geralmente fazem isso para realizar tarefas como análise de dados, automação de processos ou obter informações valiosas.

## Como fazer:

```Python
# Importando a biblioteca necessária
import requests

# Definindo a URL da página a ser baixada
url = "https://www.dados.gov.br/"

# Fazendo o download da página usando o método get() do Requests
response = requests.get(url)

# Imprimindo o conteúdo da página
print(response.text)
```
Saída de exemplo:
```
<!DOCTYPE html>
<html>
<head>
  <title>Dados.gov.br</title>
  ...
</head>
<body>
  ...
</body>
</html>
```

## Mergulho profundo:

Fazer o download de páginas da web é uma técnica comum usada pelos programadores desde o início da internet. Existem várias bibliotecas e ferramentas disponíveis para fazer isso em várias linguagens de programação. Além do método get(), o Requests também possui os métodos post() e put() para enviar dados para a web. Alguns sites podem exigir autenticação ou consentimento para fazer o download, o que pode ser tratado no código.

## Veja também:

- [Documentação do Requests](https://docs.python-requests.org/en/master/)
- [Tutorial do Real Python sobre fazer o download de páginas da web](https://realpython.com/python-requests/)
- [Artigo sobre as melhores bibliotecas do Python para fazer o download de páginas da web](https://www.pknowhow.com/blog/python-web-scraping-libraries/)