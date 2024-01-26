---
title:                "Análise de HTML"
date:                  2024-01-20T15:31:35.580204-07:00
html_title:           "Bash: Análise de HTML"
simple_title:         "Análise de HTML"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/parsing-html.md"
---

{{< edit_this_page >}}

## O Que é & Porquê?
Parsear HTML é o processo de extrair informações específicas de um documento HTML. Programadores fazem isso para automatizar a coleta de dados, manipular conteúdos de páginas web e integrar diferentes sistemas.

## Como fazer:
Em Fish Shell, não temos ferramentas nativas específicas para parsear HTML, por isso recorremos a utilitários externos como `pup` ou `hxselect`. Vamos usar `pup` como exemplo.

Instale o `pup` (assumindo que você está usando Homebrew):
```Fish Shell
brew install pup
```

Suponha que você tenha um arquivo HTML `pagina.html` e quer extrair todos os títulos:

```Fish Shell
cat pagina.html | pup 'h1 text{}'
```

Se você quiser salvar o resultado em um arquivo:

```Fish Shell
cat pagina.html | pup 'h1 text{}' > titulos.txt
```

Mostrando os títulos extraídos:
```Fish Shell
cat titulos.txt
```

Saída de exemplo:
```
Olá Mundo
Exemplo de Título
Bem-vindo à Fish Shell
```

## Mergulho Profundo:
Parsear HTML em Fish Shell normalmente requer ferramentas de terceiros, já que o shell em si não tem recursos inerentes para manipulação de HTML. `pup` é uma ferramenta minimalista de linha de comando para processar HTML, similar ao `jq` para JSON. `hxselect` é outra ferramenta que vem com `html-xml-utils` e permite seleções no estilo CSS.

Historicamente, parsear HTML era comum em linguagens como Python ou PHP, mas com a ascensão de APIs RESTful e formatos de dados como JSON, tornou-se menos frequente. Contudo, ainda é crucial para web scraping e automação onde APIs não estão disponíveis.

Quando se trata de implementação, ao escolher uma ferramenta de parseamento, considere a robustez, a suportabilidade de padrões web e a facilidade de uso.

## Veja Também:
- Documentação do `pup`: https://github.com/ericchiang/pup
- Tutorial de `hxselect`: https://www.w3.org/Tools/HTML-XML-utils/
- Sobre web scraping com Fish Shell (em inglês): https://github.com/fish-shell/fish-shell/wiki/Scripts#web-scraping
- Guia de seletores CSS (em português): https://developer.mozilla.org/pt-BR/docs/Web/CSS/CSS_Selectors
