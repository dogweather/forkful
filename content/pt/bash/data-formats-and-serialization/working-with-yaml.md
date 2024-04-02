---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:41.039497-07:00
description: "YAML, que significa YAML Ain't Markup Language (YAML N\xE3o \xE9 Uma\
  \ Linguagem de Marca\xE7\xE3o), \xE9 um padr\xE3o de serializa\xE7\xE3o de dados\
  \ leg\xEDvel por humanos que pode\u2026"
lastmod: '2024-03-13T22:44:46.773528-06:00'
model: gpt-4-0125-preview
summary: "YAML, que significa YAML Ain't Markup Language (YAML N\xE3o \xE9 Uma Linguagem\
  \ de Marca\xE7\xE3o), \xE9 um padr\xE3o de serializa\xE7\xE3o de dados leg\xEDvel\
  \ por humanos que pode\u2026"
title: Trabalhando com YAML
weight: 41
---

## O Que & Por Quê?

YAML, que significa YAML Ain't Markup Language (YAML Não é Uma Linguagem de Marcação), é um padrão de serialização de dados legível por humanos que pode ser usado para arquivos de configuração, bem como em aplicações onde os dados estão sendo armazenados ou transmitidos. Programadores gravitam em direção ao YAML devido à sua clareza e simplicidade, especialmente em projetos que envolvem configurações complexas ou a necessidade de estruturas de dados facilmente editáveis.

## Como fazer:

Trabalhar diretamente com YAML no Bash requer um pouco de engenhosidade, já que o Bash não tem suporte embutido para a análise de YAML. No entanto, você pode usar ferramentas externas como `yq` (um processador de YAML de linha de comando leve e portátil) para interagir eficientemente com arquivos YAML. Vamos passar por algumas operações comuns:

### Instalando `yq`:

Antes de mergulhar nos exemplos, certifique-se de ter `yq` instalado. Você geralmente pode instalá-lo a partir do seu gerenciador de pacotes, por exemplo, no Ubuntu:

```bash
sudo apt-get install yq
```

Ou você pode baixá-lo diretamente do seu repositório no GitHub.

### Lendo um valor:

Considere que você tenha um arquivo chamado `config.yaml` com o seguinte conteúdo:

```yaml
database:
  host: localhost
  port: 5432
user:
  name: admin
  password: secret
```

Para ler o host do banco de dados, você pode usar `yq` da seguinte forma:

```bash
yq e '.database.host' config.yaml
```

**Saída de Exemplo:**

```
localhost
```

### Atualizando um valor:

Para atualizar o nome do usuário em `config.yaml`, use o comando `yq eval` com a opção `-i` (in-place):

```bash
yq e '.user.name = "newadmin"' -i config.yaml
```

Verifique a mudança com:

```bash
yq e '.user.name' config.yaml
```

**Saída de Exemplo:**

```
newadmin
```

### Adicionando um novo elemento:

Para adicionar um novo elemento na seção do banco de dados, como um novo campo `timeout`:

```bash
yq e '.database.timeout = 30' -i config.yaml
```

Checar o conteúdo do arquivo confirmará a adição.

### Deletando um elemento:

Para remover a senha sob usuário:

```bash
yq e 'del(.user.password)' -i config.yaml
```

Esta operação removerá o campo da senha da configuração.

Lembre-se, `yq` é uma ferramenta poderosa e possui muito mais capacidades, incluindo converter YAML para JSON, mesclar arquivos e até manipulações mais complexas. Consulte a documentação do `yq` para uma exploração mais aprofundada.
