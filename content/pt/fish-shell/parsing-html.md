---
title:                "Análise de HTML"
html_title:           "Fish Shell: Análise de HTML"
simple_title:         "Análise de HTML"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/parsing-html.md"
---

{{< edit_this_page >}}

## Por que?

Você já se deparou com a tarefa de extrair informações específicas de uma página da web? Talvez você esteja criando um programa que precisa coletar dados de um site. É aí que a análise de HTML se torna útil. Ao parsear HTML, você pode extrair facilmente os dados desejados de uma página da web.

## Como fazer

Para realizar a análise de HTML no Fish Shell, vamos utilizar o utilitário "html-parsing" disponível no Fisherman. Primeiro, certifique-se de ter o Fisherman instalado em seu sistema e, em seguida, execute o seguinte comando no terminal:

```Fish Shell
fisher install html-parsing
```

Com o html-parsing instalado, agora podemos usá-lo para analisar o HTML de uma página da web. Por exemplo, vamos supor que queremos extrair o título de uma página. Podemos fazer isso da seguinte maneira:

```Fish Shell
title = curl https://www.example.com/ | html-parsing ".title" | string trim
echo $title
```

Neste exemplo, usamos o comando "curl" para obter o código HTML da página e, em seguida, usamos o html-parsing para extrair o elemento "title" e, finalmente, usamos o comando "string trim" para remover quaisquer espaços em branco adicionais. 

O mesmo pode ser feito para extrair qualquer outro elemento ou atributo de uma página da web. Basta fornecer o seletor adequado ao comando "html-parsing".

## Aprofundando

O Fish Shell usa o utilitário "libtidy" para fazer a análise de HTML. Portanto, os elementos e atributos suportados pelo libtidy também serão suportados pelo html-parsing. Para uma lista completa de seletores e atributos suportados, consulte a documentação do libtidy.

Além disso, é possível usar o html-parsing em scripts do Fish Shell para automatizar a análise de várias páginas. Com ele, você pode criar programas poderosos que podem lidar com dados provenientes de diferentes fontes na web.

## Veja também

- Documentação do html-parsing: https://github.com/oh-my-fish/plugin-html-parsing
- Documentação do libtidy: https://www.html-tidy.org/documentation/
- Fisherman: https://fisherman.github.io/