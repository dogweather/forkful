---
title:                "Fish Shell: Buscando e substituindo texto"
simple_title:         "Buscando e substituindo texto"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que utilizar busca e substituição de texto na Shell Fish?

A busca e substituição de texto é uma ferramenta poderosa na programação que permite aos usuários localizar e alterar determinados trechos de texto em um arquivo ou diretório. Isso pode economizar muito tempo e esforço, especialmente em projetos grandes com muitos arquivos. A Shell Fish possui uma função integrada embutida que facilita a realização da busca e substituição de texto.

## Como fazer busca e substituição de texto na Shell Fish?

Para utilizar a função de busca e substituição de texto na Shell Fish, primeiro você precisa abrir o terminal e navegar até o diretório onde está o arquivo que deseja alterar. Em seguida, digite o seguinte comando:

```Shell Fish
sed 's/palavra_antiga/nova_palavra' arquivo.txt
```

Isso irá substituir a "palavra_antiga" pela "nova_palavra" no arquivo "arquivo.txt". Você também pode usar curingas para substituir todas as ocorrências da palavra antiga. Por exemplo, se quiser substituir todas as letras "a" por "e" no arquivo, você pode usar o seguinte comando:

```Shell Fish
sed 's/a/e/g' arquivo.txt
```

## Explorando mais profundamente a busca e substituição de texto na Shell Fish

Além do uso básico de substituição de texto que foi mostrado acima, a Shell Fish também oferece outras opções e comandos úteis. Por exemplo, você pode especificar qual linha ou intervalo de linhas deseja substituir usando números de linha. Você também pode salvar as alterações feitas em um novo arquivo, em vez de substituir o arquivo original.

Para saber mais sobre todas as opções disponíveis e a sintaxe correta para a busca e substituição de texto na Shell Fish, você pode consultar a documentação oficial ou pesquisar por tutoriais online.

## Veja também

- Documentação oficial da Shell Fish: https://fishshell.com/docs/current/
- Tutorial de busca e substituição de texto na Shell Fish: http://fishshell.net/docs/current/tutorial.html#substitution