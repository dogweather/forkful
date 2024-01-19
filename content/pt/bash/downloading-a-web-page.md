---
title:                "Baixando uma página da web"
html_title:           "Bash: Baixando uma página da web"
simple_title:         "Baixando uma página da web"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Descarregar uma página web é o acto de extrair informação a partir de uma localização online, geralmente em formato HTML. Programadores fazem isto para usar os dados numa variedade de aplicações, desde o rastreamento de informação até à criação de repositórios offline.

## Como fazer:

Utiliza o seguinte código do **Bash** para descarregar uma página web, substituindo o URL pelo que quiseres descarregar.

```Bash
#!/bin/bash

URL="<o URL da tua escolha>"
wget $URL
```

Por exemplo, para descarregar a página inicial do Google:

```Bash
#!/bin/bash

URL="https://www.google.com"
wget $URL
```

Isto irá criar um ficheiro chamado 'index.html' no teu diretório atual.

## Mergulho Profundo

O `wget`, usado acima, é, na verdade, parte de uma longa história de ferramentas criadas para interagir com a internet desde os primórdios. Criada em 1996, é uma das aplicações CLI (Command Line Interface) mais antigas e confiáveis para descarregar conteúdo da web.

Existem alternativas ao `wget`. `curl` é outra ferramenta de linha de comando popular, enquanto que `httrack` fornece uma opção mais robusta para descarregar sites inteiros.

Sobre detalhes de implementação, quando utilizas o `wget`, o que realmente acontece é que a ferramenta envia um pedido HTTP GET para o servidor que hospeda o site. O servidor, depois, envia os dados da página de volta ao `wget`, que, por sua vez, escreve esses dados num ficheiro.

## Veja Também

- [Página manual do`wget`](https://manpages.debian.org/wget)
- [Tutorial do `curl` no Mozilla Developer Network](https://developer.mozilla.org/pt-BR/docs/Web/HTTP/CORS)
- [Site oficial `httrack`](https://www.httrack.com)
  
Não há necessidade de uma secção de "Conclusão". O importante é aprender e fazer!