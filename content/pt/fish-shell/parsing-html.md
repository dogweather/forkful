---
title:                "Analisando HTML"
aliases:
- pt/fish-shell/parsing-html.md
date:                  2024-02-03T19:12:13.600128-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analisando HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Quê?

Analisar HTML é sobre extrair dados ou informações de conteúdo HTML, uma tarefa comum ao lidar com dados da web. Programadores fazem isso para automatizar a extração de informações de sites, para tarefas como raspagem de web, mineração de dados ou testes automatizados.

## Como Fazer:

O shell Fish, predominantemente, não é projetado para analisar HTML diretamente. No entanto, ele se destaca em juntar ferramentas Unix como `curl`, `grep`, `sed`, `awk`, ou usando ferramentas especializadas como `pup` ou `beautifulsoup` em um script Python. Abaixo estão exemplos que mostram como aproveitar essas ferramentas dentro do Fish shell para analisar HTML.

### Usando `curl` e `grep`:
Buscando conteúdo HTML e extraindo linhas que contêm links:

```fish
curl -s https://exemplo.com | grep -oP '(?<=href=")[^"]*'
```

Saída:
```
/page1.html
/page2.html
...
```

### Usando `pup` (uma ferramenta de linha de comando para analisar HTML):

Primeiro, garanta que o `pup` esteja instalado. Então você pode usá-lo para extrair elementos por suas tags, ids, classes, etc.

```fish
curl -s https://exemplo.com | pup 'a attr{href}'
```

A saída, similar ao exemplo do `grep`, listaria atributos href de tags `<a>`.

### Com um script Python e `beautifulsoup`:

Embora o Fish em si não possa analisar HTML nativamente, ele se integra perfeitamente com scripts Python. Abaixo está um exemplo conciso que usa Python com `BeautifulSoup` para analisar e extrair títulos do HTML. Garanta que você tenha `beautifulsoup4` e `requests` instalados no seu ambiente Python.

**parse_html.fish**

```fish
function parse_html -a url
    python -c "
import sys
import requests
from bs4 import BeautifulSoup

response = requests.get(sys.argv[1])
soup = BeautifulSoup(response.text, 'html.parser')

titles = soup.find_all('title')

for title in titles:
    print(title.get_text())
" $url
end
```

Uso:

```fish
parse_html 'https://exemplo.com'
```

Saída:
```
Exemplo de Domínio
```

Cada um desses métodos serve diferentes casos de uso e escalas de complexidade, desde simples manipulações de texto de linha de comando até o pleno poder de análise do `beautifulsoup` em scripts Python. Dependendo das suas necessidades e da complexidade da estrutura do HTML, você pode escolher um pipeline Unix direto ou uma abordagem de script mais poderosa.
