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

## Porquê

Se você é um aspirante a desenvolvedor ou está curioso sobre como as coisas funcionam nos bastidores da internet, então aprender a baixar páginas da web pode ser um ótimo lugar para começar. Além disso, pode ser útil para quem deseja criar um banco de dados local de informações disponíveis na internet.

## Como Fazer

Baixar uma página da web em Python é muito fácil e pode ser feito em apenas algumas linhas de código. Primeiro, você precisará importar o módulo "urllib.request", que contém funções para manipular URLs. Em seguida, utilize a função "urllib.request.urlretrieve()" para baixar a página e salvá-la em um arquivo local. Veja o exemplo abaixo:

```Python
# Importando o módulo "urllib.request"
import urllib.request

# Definindo o URL da página que será baixada
url = "https://www.example.com"

# Utilizando a função "urlretrieve()" para baixar a página e salvá-la como "example.html"
urllib.request.urlretrieve(url, "example.html")

# Imprimindo uma mensagem de sucesso
print("Página baixada com sucesso!")
```

Com isso, a página será baixada e salva no mesmo diretório em que o script Python está sendo executado. Você também pode especificar um caminho e nome de arquivo diferentes, se desejar.

## Mergulho Profundo

Além da função "urlretrieve()", o módulo "urllib.request" também oferece outras funções úteis para trabalhar com URLs. Por exemplo, você pode usar a função "urlopen()" para abrir uma conexão com um URL e depois ler o conteúdo da página utilizando o método "read()". Você também pode especificar cabeçalhos HTTP personalizados, além de autenticação de usuário e senha, se necessário.

Além disso, você também pode baixar arquivos de imagem, PDFs e outros tipos de mídia utilizando a função "urlretrieve()". Basta fornecer o URL do arquivo desejado e um nome de arquivo válido.

### Veja Também

- Documentação do módulo "urllib.request": https://docs.python.org/3/library/urllib.request.html
- Tutorial sobre web scraping com Python: https://realpython.com/python-web-scraping-practical-introduction/