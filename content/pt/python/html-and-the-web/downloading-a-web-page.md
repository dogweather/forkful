---
title:                "Baixando uma página da web"
date:                  2024-01-20T17:44:44.354413-07:00
model:                 gpt-4-1106-preview
simple_title:         "Baixando uma página da web"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/downloading-a-web-page.md"
---

{{< edit_this_page >}}

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
