---
date: 2024-01-20 17:57:37.309591-07:00
description: "Procurar e substituir texto \xE9 reescrever partes espec\xEDficas de\
  \ um documento sem alterar o resto. Programadores fazem isso para corrigir erros,\
  \ atualizar\u2026"
lastmod: '2024-03-13T22:44:46.736298-06:00'
model: gpt-4-1106-preview
summary: "Procurar e substituir texto \xE9 reescrever partes espec\xEDficas de um\
  \ documento sem alterar o resto. Programadores fazem isso para corrigir erros, atualizar\u2026"
title: Pesquisando e substituindo texto
---

{{< edit_this_page >}}

## O Que é & Porquê?
Procurar e substituir texto é reescrever partes específicas de um documento sem alterar o resto. Programadores fazem isso para corrigir erros, atualizar informações ou alterar a configuração de forma eficiente em múltiplos arquivos.

## Como Fazer:
Vamos usar o `sed`, um editor de fluxo para processamento de texto:

```Bash
# Substituindo "gato" por "cão" no arquivo animais.txt
sed 's/gato/cão/' animais.txt

# Para garantir que a substituição seja realizada em todas as ocorrências, use o sinalizador 'g' (global):
sed 's/gato/cão/g' animais.txt

# Caso queira salvar as alterações no arquivo, utilize a opção -i:
sed -i 's/gato/cão/g' animais.txt

# Exemplo de output:
# Antes: Eu tenho um gato e mais dois gatos.
# Depois: Eu tenho um cão e mais dois cães.
```

## Mergulho Profundo:
O `sed`, abreviatura de Stream Editor, é uma ferramenta que existe desde os primeiros dias do Unix. Foi uma evolução dos editores de linha como `ed`, desenhado para filtrar e transformar texto de maneira programática.

Alternativas abrangem ferramentas como `awk`, que é mais poderoso para operações baseadas em padrões e campos, e linguagens de script como `perl` e `python`, que oferecem bibliotecas robustas para expressões regulares e processamento de texto.

A implementação do `sed` é baseada em expressões regulares, um método poderoso para especificar padrões de texto que facilita substituições complexas em grande escala.

## Veja Também:
- [GNU sed manual](https://www.gnu.org/software/sed/manual/sed.html)
- [Regular Expressions Quick Start](https://www.regular-expressions.info/quickstart.html)
- [The AWK Manual](https://www.gnu.org/software/gawk/manual/gawk.html)
- [Perl Text Processing](https://perldoc.perl.org/index-functions-by-cat.html)
