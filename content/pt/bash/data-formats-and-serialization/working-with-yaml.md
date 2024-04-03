---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:41.039497-07:00
description: "Como fazer: Trabalhar diretamente com YAML no Bash requer um pouco de\
  \ engenhosidade, j\xE1 que o Bash n\xE3o tem suporte embutido para a an\xE1lise\
  \ de YAML. No\u2026"
lastmod: '2024-03-13T22:44:46.773528-06:00'
model: gpt-4-0125-preview
summary: "Trabalhar diretamente com YAML no Bash requer um pouco de engenhosidade,\
  \ j\xE1 que o Bash n\xE3o tem suporte embutido para a an\xE1lise de YAML."
title: Trabalhando com YAML
weight: 41
---

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
