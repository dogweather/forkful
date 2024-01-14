---
title:                "Python: Baixando uma página da web"
simple_title:         "Baixando uma página da web"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Por que baixar uma página da web?

Baixar uma página da web pode ser útil em várias situações, como, por exemplo, para realizar análises de dados, extrair informações de um site ou criar um arquivo local para visualização offline.

## Como fazer

Para baixar uma página da web em Python, podemos utilizar a biblioteca "requests". Primeiro, importamos a biblioteca:

```Python
import requests
```

Em seguida, utilizamos a função "get" da biblioteca para fazer a requisição da página que queremos baixar, passando o URL como parâmetro:

```Python
page = requests.get('https://www.exemplo.com')
```

Para salvar o conteúdo da página em um arquivo local, podemos utilizar o método "write" do objeto "page", juntamente com a função "open" para criar e abrir um arquivo em modo de escrita:

```Python
with open("arquivo.html", "w") as file:
    file.write(page.text)
```

Pronto! Agora temos um arquivo "arquivo.html" com o conteúdo da página baixada. Podemos utilizar bibliotecas como "BeautifulSoup" para fazer o parsing do conteúdo e extrair informações específicas.

## Mergulho profundo

Quando fazemos uma requisição da página, recebemos uma resposta do servidor, que contém informações como o status da requisição e o conteúdo da página. Podemos acessar essas informações no objeto "page". Por exemplo, podemos verificar o status da requisição:

```Python
print(page.status_code) # imprime o código de status da requisição
```

Também podemos acessar os cabeçalhos da resposta:

```Python
print(page.headers) # imprime os cabeçalhos da resposta
```

Podemos ainda adicionar cabeçalhos personalizados em nossa requisição, utilizando o parâmetro "headers" da função "get". Isso pode ser útil quando queremos simular o acesso de um navegador:

```Python
page = requests.get('https://www.exemplo.com', headers={'User-Agent': 'Mozilla/5.0'}) # simula o acesso de um navegador
```

## Veja também

- [Documentação da biblioteca "requests"](https://requests.readthedocs.io)
- [Documentação da biblioteca "BeautifulSoup"](https://www.crummy.com/software/BeautifulSoup/bs4/doc)