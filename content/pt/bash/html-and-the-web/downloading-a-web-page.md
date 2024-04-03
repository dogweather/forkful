---
date: 2024-01-20 17:43:37.213336-07:00
description: "Baixar uma p\xE1gina da web significa copiar o seu conte\xFAdo da internet\
  \ para o seu computador. Programadores fazem isso para analisar dados, testar\u2026"
lastmod: '2024-03-13T22:44:46.751584-06:00'
model: gpt-4-1106-preview
summary: "Baixar uma p\xE1gina da web significa copiar o seu conte\xFAdo da internet\
  \ para o seu computador."
title: "Baixando uma p\xE1gina da web"
weight: 42
---

## How to:
Para baixar uma página da web com o Bash, podemos usar o `curl` ou `wget`. Aqui estão os exemplos:

```Bash
# Usando curl
curl http://example.com -o example.html

# Saída esperada: Nenhuma, se tudo correr bem, o arquivo example.html será criado com o conteúdo da página.

# Usando wget
wget http://example.com

# Saída esperada: O wget irá mostrar o progresso do download e salvará a página como index.html por padrão.
```

## Deep Dive
Historicamente, o `wget` está por aí desde 1996, com a ideia de ser uma ferramenta para recuperar conteúdo da web via linha de comando. O `curl` veio logo depois, em 1997, oferecendo mais protocolos e uma biblioteca (libcurl) para os desenvolvedores.

Existem alternativas. Por exemplo, você pode usar o `lynx -dump -nolist http://example.com > example.html` para baixar o texto de uma página web com o navegador de texto Lynx.

Implementar o download de uma página web também pode envolver cuidados com a etiqueta da internet, como respeitar o arquivo `robots.txt` de um site, e lidar com as questões de segurança, como a validação de certificado SSL.

## See Also
- `man curl`
- `man wget`
- [Project website for curl](https://curl.se/)
- [Project website for wget](https://www.gnu.org/software/wget/)
- [Lynx Browser](http://lynx.browser.org/)
