---
date: 2024-01-20 17:44:44.354413-07:00
description: "Baixar uma p\xE1gina da web \xE9 o processo de salvar os dados de uma\
  \ p\xE1gina em um arquivo local. Programadores fazem isso para analisar o conte\xFA\
  do, obter\u2026"
lastmod: '2024-03-13T22:44:46.152558-06:00'
model: gpt-4-1106-preview
summary: "Baixar uma p\xE1gina da web \xE9 o processo de salvar os dados de uma p\xE1\
  gina em um arquivo local."
title: "Baixando uma p\xE1gina da web"
weight: 42
---

## O Que & Por Quê?

Baixar uma página da web é o processo de salvar os dados de uma página em um arquivo local. Programadores fazem isso para analisar o conteúdo, obter informações ou monitorar alterações.

## Como Fazer:

Para baixar uma página da web em Python, você pode usar a biblioteca `requests`. Veja como:

```Python
import requests

# Endereço da página que você quer baixar
url = 'http://exemplo.com'

# Realiza um GET request
resposta = requests.get(url)

# Checa se o request foi bem-sucedido
if resposta.status_code == 200:
    # Salva o conteúdo da página em um arquivo
    with open('pagina.html', 'w', encoding='utf-8') as arquivo:
        arquivo.write(resposta.text)

# Imprime o resultado
print('Página baixada!')
```

Saída esperada:

```
Página baixada!
```

## Mergulho Profundo

Antes da biblioteca `requests`, o módulo `urllib` era comumente usado para tarefas relacionadas à internet. Porém, a `requests` se tornou mais popular devido à sua simplicidade e facilidade de uso. Ela abstrai muitos detalhes de implementação, tornando o ato de fazer requests HTTP quase trivial.

Além de simples GET requests, `requests` também permite fazer POST, PUT, DELETE e outros tipos de requests HTTP com a mesma facilidade. Ao baixar páginas, tenha em mente quesitos como respeito ao `robots.txt` da página e leis de direitos autorais.

Uma alternativa ao `requests` é utilizar o Selenium ou o Beautiful Soup se você precisar de mais controle sobre elementos dinâmicos da página ou quando precisar de parsing de HTML respectivamente.

## Veja Também

- Documentação oficial do `requests`: https://docs.python-requests.org/en/latest/
- Módulo `urllib`: https://docs.python.org/3/library/urllib.html
- Beautiful Soup para parsing de HTML: https://www.crummy.com/software/BeautifulSoup/bs4/doc/
- Selenium para navegação automatizada em páginas web: https://selenium-python.readthedocs.io/
