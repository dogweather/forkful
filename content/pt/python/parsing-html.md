---
title:                "Python: Analisando html"
simple_title:         "Analisando html"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/parsing-html.md"
---

{{< edit_this_page >}}

## Por que

Para usuários do Python, o parsing HTML (análise de HTML) pode ser uma habilidade útil para extrair dados de páginas da web. Isso pode ser usado para fins de scraping ou para automatizar tarefas como a coleta de informações de múltiplas páginas.

## Como Fazer

O Python possui duas bibliotecas populares para analisar HTML: Beautiful Soup e lxml. Aqui está um exemplo de como usar o Beautiful Soup para extrair todos os links de uma página:

```Python
from bs4 import BeautifulSoup
import requests

# Obtém o conteúdo da página
url = "https://www.example.com"
page = requests.get(url)
content = page.content

# Analisa o conteúdo usando o Beautiful Soup
soup = BeautifulSoup(content, "html.parser")

# Encontra todos os links no conteúdo
links = soup.find_all("a")

# Imprime todos os links encontrados
for link in links:
    print(link.get("href"))
```

**Saída de Exemplo:**

```
https://www.example.com
https://www.example.com/about
https://www.example.com/contact
```

## Deep Dive

O Beautiful Soup usa uma técnica de parsing chamada de árvore DOM (Document Object Model) para encontrar e manipular elementos HTML. A biblioteca também trata automaticamente coisas como encoding e manipulação de dados mal formados.

Além disso, há muitos métodos e atributos disponíveis no Beautiful Soup para ajudar na navegação e extração de dados de páginas complexas. É útil ler a documentação e explorar diferentes formas de usar a biblioteca para tarefas específicas.

## Veja Também

- [Documentação do Beautiful Soup](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)
- [Tutorial do Beautiful Soup](https://www.dataquest.io/blog/web-scraping-tutorial-python/)
- [Documentação do lxml](https://lxml.de/)
- [Introdução ao parsing HTML com Python](https://realpython.com/python-web-scraping-practical-introduction/)