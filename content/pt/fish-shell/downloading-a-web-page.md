---
title:                "Baixando uma página da web"
html_title:           "Bash: Baixando uma página da web"
simple_title:         "Baixando uma página da web"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## O que & Por quê?

Fazer o download de uma página da web consiste em recuperar e guardar o conteúdo HTML dessa página em seu sistema local. Programadores fazem isso para analisar o conteúdo de uma página da web, para extração de dados (webscraping) ou até mesmo para testar a funcionalidade de um site.

 
## Como fazer:

Para fazer o download de uma página web no Fish Shell, você pode usar o comando `curl`. Veja abaixo um exemplo:

```Fish Shell
curl -o pagina_web.html https://exemplo.com
```

No exemplo acima, `curl` é a ferramenta que estamos usando para fazer o download da página web, o parâmetro `-o` permite especificar o nome do arquivo para o qual o conteúdo será baixado (`pagina_web.html`), e `https://exemplo.com` é a URL da página que desejamos baixar.

Aqui está a saída de exemplo:

```Fish Shell
% Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
100   127  100   127    0     0    425      0 --:--:-- --:--:-- --:--:--   425
```

## Mergulho Profundo:

Fazer download de páginas web é uma prática antiga, datando da época antes de APIs e serviços modernos para compartilhamento de dados. Porém, ainda é relevante hoje para muitas aplicações.

Existem muitas alternativas para baixar uma página web além do `curl`, como o `wget` e o `httpie`. Cada um desses tem suas próprias vantagens e desvantagens, então é bom conhecer todas as opções disponíveis.

Os detalhes de implementação sobre o download de uma página web podem variar dependendo da ferramenta que você está usando. O `curl`, por exemplo, suporta uma série de protocolos além do HTTP, como FTP e MAILTO.

## Veja Também:

1. "Curl vs Wget: O que é melhor?" -> (link opcional)
2. "Uma introdução ao HTTPie" -> (link opcional)
3. Documentação oficial do `curl`: https://curl.haxx.se/docs/manpage.html