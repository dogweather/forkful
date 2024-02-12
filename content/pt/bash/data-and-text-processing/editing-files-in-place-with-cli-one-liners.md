---
title:                "Editando arquivos in loco com linhas de comando"
aliases:
- /pt/bash/editing-files-in-place-with-cli-one-liners.md
date:                  2024-01-27T16:20:50.596326-07:00
model:                 gpt-4-0125-preview
simple_title:         "Editando arquivos in loco com linhas de comando"

tag:                  "Data and Text Processing"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/editing-files-in-place-with-cli-one-liners.md"
---

{{< edit_this_page >}}

## O Quê & Por Quê?

Imagine que você acabou de descobrir que precisa fazer uma atualização em lote em vários arquivos de configuração no seu servidor. Você poderia abrir cada arquivo, fazer as alterações manualmente e salvá-las. Ou, você pode realizar edições no local diretamente da sua interface de linha de comando (CLI), uma habilidade que economiza tempo, reduz erros e automatiza tarefas repetitivas. Essa técnica é especialmente útil para atualizações sistêmicas, correções ou modificações em massa onde as edições manuais poderiam ser impraticáveis ou propensas a erros.

## Como Fazer:

Quando se trata de editar arquivos no local usando Bash, duas ferramentas proeminentes entram em jogo: `sed` e `awk`. Vamos explorar como usar essas poderosas utilidades com alguns exemplos de código.

### Usando `sed` para substituição de texto simples

O seguinte comando substitui a primeira ocorrência de "text1" por "text2" em `file.txt`:

```Bash
sed -i 's/text1/text2/' file.txt
```

Para uma substituição global (todas as ocorrências), você adicionaria um `g` no final:

```Bash
sed -i 's/text1/text2/g' file.txt
```

Para modificar vários arquivos de uma vez:

```Bash
sed -i 's/text1/text2/g' file1.txt file2.txt file3.txt
```

### Usando `awk` para manipulações mais complexas

`awk` é outra ferramenta que se destaca com suas capacidades de programação, especialmente útil para o processamento de texto que envolve dados baseados em campos.

Alterando o segundo campo de cada linha para `newValue` em `data.csv`, separado por vírgulas:

```Bash
awk -i inplace -F, '{$2="newValue"; print $0}' OFS=, data.csv
```

### Faça backup antes de pular

Um conselho prático: sempre crie um backup antes da edição no local. `sed` facilita isso com a opção `-i` seguida de um sufixo para criar um backup.

```Bash
sed -i.bak 's/text1/text2/g' file.txt
```

Este comando cria um backup do `file.txt` original como `file.txt.bak` antes de realizar a substituição.

## Mergulho Profundo

A capacidade de editar arquivos diretamente da linha de comando surgiu como uma progressão natural da filosofia Unix: empoderando usuários para gerenciar e manipular dados de forma eficiente com o mínimo de teclas possível. No entanto, esse poder vem com suas ressalvas.

### Contexto histórico

Ferramentas Unix como `sed` e `awk` existem desde os primeiros dias do Unix, criadas como parte de sua filosofia de ferramentas, focando em comandos especializados e compostos. Sua inclusão no arsenal do Unix foi uma resposta à necessidade de processamento de texto eficiente em uma paisagem dominada por interfaces de linha de comando.

### Alternativas

Enquanto `sed` e `awk` são poderosos, eles não são as únicas opções. Perl e Python, por exemplo, têm opções de linha de comando (`-p` e `-i`, respectivamente) que permitem capacidades similares de edição no local com uma sintaxe possivelmente mais legível para operações complexas.

```Bash
perl -pi -e 's/text1/text2/g' file.txt
```

```Bash
python -c "import fileinput, sys; [sys.stdout.write(line.replace('text1', 'text2')) for line in fileinput.input(files='file.txt', inplace=True)]"
```

Cada alternativa tem suas forças: as capacidades de uma linha de Perl são imensas, e a sintaxe de Python é possivelmente mais acessível para aqueles não profundamente versados em ferramentas de processamento de texto Unix.

### Detalhes de implementação

Edição no local não é verdadeiramente "no local" em um sentido técnico. Tanto `sed -i` quanto `awk -i inplace` trabalham criando um arquivo temporário no qual a saída processada é armazenada antes de substituir o arquivo original. Esta abordagem garante que o arquivo não seja corrompido caso o processo seja interrompido. As implicações são principalmente em recursos e permissões: você deve ter espaço em disco suficiente para o arquivo temporário e as permissões para criar arquivos no diretório do seu arquivo de destino.

Embora poderosos, comandos de edição no local devem ser usados com cautela. Uma regex mal colocada pode resultar em perda de dados, enfatizando a importância dos backups. Apesar dos possíveis contratempos, dominar esses comandos pode aumentar significativamente sua capacidade de realizar modificações de arquivos rápidas e eficientes diretamente da linha de comando, incorporando a filosofia Unix de aproveitar ferramentas simples e poderosas para realizar tarefas complexas.
