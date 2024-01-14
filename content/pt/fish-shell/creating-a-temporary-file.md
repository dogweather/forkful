---
title:    "Fish Shell: Criando um arquivo temporário"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que

Criar um arquivo temporário é uma tarefa comum em programação, especialmente na Fish Shell. Temporariamente armazenar dados ou informações pode ser útil para testar códigos ou para fins de organização.

## Como fazer

Para criar um arquivo temporário na Fish Shell, você pode seguir os seguintes passos:

1. Abra o terminal e inicie a Fish Shell digitando `fish`.
2. Digite `mktemp` seguido do nome do arquivo temporário que você deseja criar, por exemplo `tmp_file`.
3. Para escrever algum conteúdo no arquivo, use o redirecionamento de saída `>` seguido pelo nome do arquivo: `echo "Conteúdo do arquivo" > tmp_file`.
4. Agora você pode verificar a existência do arquivo temporário com `ls`, que deve exibir o arquivo `tmp_file`.

Código completo:

```
fish
mktemp tmp_file
echo "Conteúdo do arquivo" > tmp_file
ls
```

Saída:

```
tmp_file
```

## Deep Dive

Existem algumas coisas a serem consideradas ao criar um arquivo temporário na Fish Shell. Por padrão, o arquivo temporário será criado dentro do diretório temporário padrão do usuário (`/tmp`). No entanto, você pode especificar um diretório diferente usando a variável de ambiente `$TMPDIR`.

Além disso, é importante lembrar de excluir o arquivo temporário após seu uso, para não deixar resíduos desnecessários no sistema. Você pode fazer isso manualmente com o comando `rm`, ou incluir esse comando no seu script para garantir que o arquivo seja excluído automaticamente.

## Veja também

- [Tutorial de redirecionamento de saída no Fish Shell](https://fishshell.com/docs/current/tutorial.html#output-redirection)
- [Documentação completa da Fish Shell](https://fishshell.com/docs/current/index.html#generate-a-temporary-file-using-mktemp)