---
date: 2024-01-20 17:44:04.387439-07:00
description: "Baixar uma p\xE1gina da web \xE9, basicamente, capturar todo o conte\xFA\
  do de uma URL para que voc\xEA possa v\xEA-lo offline ou manipul\xE1-lo programaticamente.\u2026"
lastmod: '2024-03-11T00:14:20.743667-06:00'
model: gpt-4-1106-preview
summary: "Baixar uma p\xE1gina da web \xE9, basicamente, capturar todo o conte\xFA\
  do de uma URL para que voc\xEA possa v\xEA-lo offline ou manipul\xE1-lo programaticamente.\u2026"
title: "Baixando uma p\xE1gina da web"
---

{{< edit_this_page >}}

## O Que é & Por Que?
Baixar uma página da web é, basicamente, capturar todo o conteúdo de uma URL para que você possa vê-lo offline ou manipulá-lo programaticamente. Programadores fazem isso para automatizar a coleta de dados, testar aplicações web ou simplesmente arquivar informações.

## Como Fazer:
Vamos usar o `curl`, um comando poderoso para transferência de dados. No Fish, isso fica simples assim:

```Fish Shell
# Baixa o conteúdo da página inicial do Google e salva no arquivo google.html
curl https://www.google.com -o google.html
```

Resultado esperado é um arquivo chamado `google.html` no seu diretório atual contendo o HTML do Google.

## Mergulho Profundo
O ato de baixar páginas da web data dos primeiros dias da internet. Antes do `curl`, que apareceu pela primeira vez em 1997, os protocolos FTP e HTTP eram manipulados por softwares mais primitivos e específicos. O `curl` eventualmente se tornou um dos favoritos por sua simplicidade e versatilidade.

Além do `curl`, existem outras ferramentas, como o `wget`, que é particularmente bom para baixar o conteúdo inteiro de sites. No entanto, o `curl` ainda é preferido para operações rápidas de transferência de dados devido à sua sintaxe simplificada e à ampla disponibilidade em diferentes sistemas operacionais.

A implementação exata das ferramentas pode variar dependendo da versão, mas o conceito central é o mesmo: requisitar os dados de uma URL e salvá-los localmente. É possível também que uma linguagem de programação como Python ou Ruby tenha suas próprias bibliotecas para baixar páginas web, mas para muitos scripts rápidos e sujos, `curl` no shell é a maneira mais rápida e fácil de fazer o trabalho.

## Veja Também

- Documentação do `curl`: https://curl.se/docs/
- Uma comparação entre `curl` e `wget`: https://www.baeldung.com/linux/wget-vs-curl
- Tutorial sobre automatização de downloads com o Fish shell: https://fishshell.com/docs/current/tutorial.html#tut_automatic_web_downloads
