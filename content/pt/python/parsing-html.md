---
title:                "Análise de HTML"
html_title:           "Python: Análise de HTML"
simple_title:         "Análise de HTML"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/parsing-html.md"
---

{{< edit_this_page >}}

## Por que

Você provavelmente já encontrou situações em que precisou extrair informações específicas de uma página da web. Talvez você quisesse encontrar o preço de um produto em um site de compras ou obter dados de uma tabela em um site de notícias. Para isso, é necessário "analisar" o HTML da página, ou seja, extrair informações específicas do código fonte da página.

## Como Fazer

A seguir, mostraremos um exemplo de como fazer isso usando a linguagem de programação Python.

Primeiro, precisamos importar o módulo "Beautiful Soup", que é uma biblioteca de Python que ajuda a analisar documentos HTML. Para fazer isso, basta digitar o seguinte código:

``` Python
from bs4 import BeautifulSoup
```

Em seguida, vamos criar uma variável com o conteúdo HTML que queremos analisar. Neste exemplo, vamos usar o código fonte de um artigo em um site de notícias:

``` Python
html = """
<!DOCTYPE html>
<html>
<head>
	<title>Exemplo de página da web</title>
</head>
<body>
	<h1>Este é um título</h1>
	<p>Este é um parágrafo de exemplo</p>
	<ul>
		<li>Item 1</li>
		<li>Item 2</li>
		<li>Item 3</li>
	</ul>
</body>
</html>
"""
```

Agora, vamos usar o BeautifulSoup para analisar o conteúdo HTML e extrair a informação que desejamos. Por exemplo, se quisermos obter o título da página, podemos usar o seguinte código:

``` Python
soup = BeautifulSoup(html, 'html.parser')
titulo = soup.title.string
print(titulo) # output: Exemplo de página da web
```

Da mesma forma, se quisermos pegar todos os itens da lista, podemos usar o seguinte código:

``` Python
lista = soup.find_all('li') # find_all encontra todos os elementos com a tag "li"
for item in lista:
  print(item.text) # output: Item 1 Item 2 Item 3
```

Esses são apenas exemplos simples, mas existem muitas outras maneiras de extrair informações específicas de páginas da web usando a biblioteca BeautifulSoup em Python.

## Deep Dive

A biblioteca BeautifulSoup é muito poderosa e oferece várias maneiras de analisar e extrair informações do HTML. Alguns dos métodos mais usados são:

- `find()`: encontra o primeiro elemento correspondente à tag especificada;
- `find_all()`: encontra todos os elementos correspondentes à tag especificada;
- `select()`: encontra os elementos usando seletores CSS;
- `get_text()`: retorna o texto contido dentro de um elemento.

Para saber mais sobre a biblioteca BeautifulSoup, você pode consultar a documentação oficial [aqui](https://www.crummy.com/software/BeautifulSoup/bs4/doc/).

## Veja também

- [Documentação oficial da biblioteca BeautifulSoup](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)
- [Tutorial completo de como usar a biblioteca BeautifulSoup](https://www.dataquest.io/blog/web-scraping-tutorial-python/)