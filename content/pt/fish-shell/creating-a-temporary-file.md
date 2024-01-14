---
title:                "Fish Shell: Criando um arquivo temporário"
programming_language: "Fish Shell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que criar um arquivo temporário usando o Fish Shell

Criar arquivos temporários é uma prática comum na programação e pode ser útil em diversas situações, como armazenar dados temporários, testar um novo código ou criar backups. No Fish Shell, essa tarefa pode ser realizada rapidamente e facilmente usando alguns comandos simples.

## Como criar um arquivo temporário usando o Fish Shell

Para criar um arquivo temporário usando o Fish Shell, basta seguir os seguintes passos:

1. Abra o Fish Shell no seu terminal.
2. Use o comando `mktemp` seguido de um nome de arquivo desejado, por exemplo: `mktemp meuarquivo.txt`.
3. O comando irá criar um arquivo temporário com o nome especificado e uma sequência de caracteres aleatórios adicionada ao final, para evitar conflitos de nomes.
4. Para editar o arquivo, use o comando `nano` seguido do nome do arquivo temporário: `nano meuarquivo.txt.[sequência aleatória]`, onde a sequência aleatória é a que foi gerada pelo comando `mktemp`.
5. Quando terminar de editar o arquivo, salve e feche o editor.
6. O arquivo temporário será automaticamente excluído quando você sair do Fish Shell.

Abaixo está um exemplo de como criar um arquivo temporário chamado "loremipsum.txt" e adicionar o texto "Lorem ipsum dolor sit amet" a ele:

```Fish Shell
mktemp loremipsum.txt
nano loremipsum.txt.[sequência aleatória]
[adicionar texto ao arquivo]
[Ctrl + O para salvar]
[Ctrl + X para sair do editor]
```

## Uma análise mais aprofundada sobre a criação de arquivos temporários

Ao criar um arquivo temporário usando o comando `mktemp`, o Fish Shell faz uso da variável de ambiente `$TMPDIR` para determinar o local onde o arquivo deve ser criado. Por padrão, essa variável é definida como `/tmp`, mas pode ser alterada pelo usuário de acordo com suas necessidades.

Além disso, o Fish Shell também permite que você especifique o sufixo do arquivo temporário usando a opção `-s` do comando `mktemp`, o que pode ser útil caso você precise diferenciar entre vários arquivos temporários criados em um mesmo diretório.

## Veja também

- [Documentação oficial do Fish Shell](https://fishshell.com/docs/current/index.html)
- [Guia rápido para iniciantes do Fish Shell](https://gist.github.com/fabiomaia/0b9c41e0830ea1e865aeaae9f6d7da98)
- [Tutorial básico de programação em Fish Shell](https://medium.com/@Naosyth/iniciando-com-o-fish-shell-87d1dd69e1a5)