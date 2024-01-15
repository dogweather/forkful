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

## Por que baixar uma página da web?

Baixar uma página da web pode ser útil para quem deseja salvar conteúdo para acesso offline ou para fins de análise ou desenvolvimento. Além disso, pode ser útil para lidar com problemas de conexão ou armazenamento em cache.

## Como fazer:

Para baixar uma página da web usando o Bash, basta seguir esses passos:

1. Abra o terminal no seu computador.
2. Use o comando `wget` seguido do URL da página que deseja baixar:

   ```Bash
   wget https://www.example.com
   ```
3. Aguarde até que o download seja concluído.
4. A página será salva em seu diretório atual com o mesmo nome do arquivo da página original.

## Aprofundando:

O comando `wget` é uma ferramenta de linha de comando usada para recuperar conteúdo de servidores web. Ele também possui recursos avançados que permitem baixar conteúdo recursivamente, agendar downloads e definir limites de largura de banda.

Além disso, é possível usar outras opções para personalizar o download, como especificar o diretório de destino e definir cabeçalhos HTTP personalizados.

## Veja também:

- [Documentação oficial do wget](https://www.gnu.org/software/wget/)
- [Guia para iniciantes em Bash](https://www.hostinger.com.br/tutoriais/comandos-basicos-do-linux/)
- [Técnicas avançadas de linha de comando para Bash](https://catonmat.net/bash-one-liners-explained-part-one)