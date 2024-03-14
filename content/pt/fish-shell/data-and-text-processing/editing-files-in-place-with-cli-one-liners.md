---
date: 2024-01-27 16:20:56.437097-07:00
description: "Editar arquivos in-loco com linhas de comando \xFAnicas (CLI one-liners)\
  \ \xE9 sobre fazer mudan\xE7as diretamente nos arquivos a partir da linha de comando,\
  \ sem\u2026"
lastmod: '2024-03-13T22:44:47.001899-06:00'
model: gpt-4-0125-preview
summary: "Editar arquivos in-loco com linhas de comando \xFAnicas (CLI one-liners)\
  \ \xE9 sobre fazer mudan\xE7as diretamente nos arquivos a partir da linha de comando,\
  \ sem\u2026"
title: Editando arquivos in loco com linhas de comando
---

{{< edit_this_page >}}

## O Que & Por Que?

Editar arquivos in-loco com linhas de comando únicas (CLI one-liners) é sobre fazer mudanças diretamente nos arquivos a partir da linha de comando, sem abrir eles em um editor de texto. Programadores fazem isso para economizar tempo e automatizar tarefas de edição repetitivas, tornando seu fluxo de trabalho mais suave e eficiente.

## Como fazer:

Fish Shell, conhecido por suas características amigáveis ao usuário e poderosas capacidades de script, oferece várias maneiras de editar arquivos in-loco. No entanto, ao contrário de alguns outros shells, Fish não tem um mecanismo embutido para edição in-loco (`sed -i` no Bash, por exemplo). Mas não tema, você ainda pode alcançar isso com um pouco de criatividade e ajuda de ferramentas externas como `sed` e `awk`.

### Usando `sed` para substituições simples
Para substituir todas as instâncias de "hello" por "world" em `file.txt`, você usaria:
```Fish Shell
sed -i '' 's/hello/world/g' file.txt
```

### Aplicando múltiplos comandos `sed`
Se você precisa realizar várias substituições, pode encadeá-las assim:
```Fish Shell
sed -i '' -e 's/fish/bass/g' -e 's/rainbow/trout/g' file.txt
```

### Usando `awk` para operações mais complexas
Para operações complexas demais para o `sed`, `awk` pode ser a ferramenta de sua escolha. Aqui está como duplicar o número em cada linha:
```Fish Shell
awk '{print $1 * 2}' file.txt > temp && mv temp file.txt
```

### Nota sobre o Tratamento de Erros
Lembre-se, ao usar essas ferramentas a partir do Fish, capturar erros e entender suas mensagens é crucial. Use o robusto tratamento de erros do Fish para fazer seus scripts mais confiáveis.

## Mergulho Profundo

Historicamente, a edição de arquivos in-loco tem sido um ponto principal da programação Unix e Linux, oferecendo uma maneira eficiente de realizar edições rápidas sem abrir manualmente os arquivos. Ferramentas como `sed` e `awk` são utilidades veneráveis que existem desde os primeiros dias do Unix, tornando-se indispensáveis para tarefas de processamento de texto.

Fish Shell, sendo mais moderno e ostentando melhorias em usabilidade e script, carece de edição in-loco embutida principalmente devido à sua filosofia de design focada em interatividade e amigável ao usuário. A ausência de um comando nativo de edição in-loco no Fish sublinha a importância das ferramentas externas nos ecossistemas semelhantes ao Unix.

Alternativas para edição in-loco no Fish incluem o uso de arquivos temporários ou aproveitando linhas únicas de Perl ou Python, que podem oferecer mais flexibilidade ou legibilidade para tarefas complexas.

Por exemplo, usando Perl:
```Fish Shell
perl -pi -e 's/find/replace/g' file.txt
```
Ou Python:
```Fish Shell
python -c "import re, sys; [sys.stdout.write(re.sub('padrão', 'substituição', line)) for line in sys.stdin]" < file.txt > temp && mv temp file.txt
```

Em termos de implementação, quando você realiza edição in-loco, por debaixo dos panos, essas ferramentas tipicamente criam um arquivo temporário, escrevem as mudanças lá e, então, substituem o arquivo original pela versão modificada. Esta abordagem garante que o processo de edição do arquivo não corrompa ou perca dados se um erro ocorrer durante a operação.

Entender essas ferramentas e métodos permite aos programadores do Fish Shell incorporar edição in-loco em seus scripts de forma eficaz, fechando a lacuna entre as características amigáveis ao usuário do Fish e o poder bruto das utilidades de processamento de texto Unix tradicionais.
