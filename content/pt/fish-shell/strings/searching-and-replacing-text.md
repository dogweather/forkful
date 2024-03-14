---
date: 2024-01-20 17:57:49.074436-07:00
description: "Pesquisar e substituir texto \xE9 a arte de encontrar peda\xE7os espec\xED\
  ficos de texto e troc\xE1-los por outros. Programadores fazem isso para corrigir\
  \ erros,\u2026"
lastmod: '2024-03-13T22:44:46.989662-06:00'
model: gpt-4-1106-preview
summary: "Pesquisar e substituir texto \xE9 a arte de encontrar peda\xE7os espec\xED\
  ficos de texto e troc\xE1-los por outros. Programadores fazem isso para corrigir\
  \ erros,\u2026"
title: Pesquisando e substituindo texto
---

{{< edit_this_page >}}

## O Que É & Porquê?
Pesquisar e substituir texto é a arte de encontrar pedaços específicos de texto e trocá-los por outros. Programadores fazem isso para corrigir erros, atualizar dados ou melhorar o código de forma rápida.

## Como Fazer:
Substituir todas as instâncias de "bacalhau" por "sardinha" num arquivo `menu.txt`:

```Fish Shell
sed 's/bacalhau/sardinha/g' menu.txt > menu_atualizado.txt
```

Substituir in-place (diretamente no arquivo) sem criar um novo arquivo:

```Fish Shell
sed -i 's/bacalhau/sardinha/g' menu.txt
```

Output:
```
# Antes
Prato do dia: bacalhau à brás
# Depois
Prato do dia: sardinha à brás
```

## Mergulho Profundo
A busca e substituição de texto é utilizada desde os primórdios da programação de computadores. O comando `sed`, por exemplo, é uma abreviação de "stream editor", utilizado desde os primeiros dias do Unix para editar fluxos de texto de forma programática. Embora o Fish Shell em si seja mais moderno, ele carrega esse legado, proporcionando uma interface acessível para poderosas ferramentas de linha de comando como `sed` e `awk`. Como alternativa ao `sed`, algumas pessoas preferem usar `perl` ou `awk` para tarefas mais complexas devido à sua expressividade e poderosos recursos de manipulação de texto. No Fish Shell, isso tudo é facilitado com uma sintaxe simplificada e recursos amigáveis que suavizam o aprendizado para novos usuários.

## Veja Também
- Documentação oficial do Fish Shell: https://fishshell.com/docs/current/index.html
- Tutorial completo de `sed`: https://www.grymoire.com/Unix/Sed.html
- Exemplos e dicas para `awk`: https://www.gnu.org/software/gawk/manual/gawk.html
