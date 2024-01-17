---
title:                "Trabalhando com yaml"
html_title:           "Bash: Trabalhando com yaml"
simple_title:         "Trabalhando com yaml"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/working-with-yaml.md"
---

{{< edit_this_page >}}

## O que & Por Que?

Trabalhar com YAML é uma forma de organizar e armazenar dados de forma estruturada em arquivos de texto. Os programadores o utilizam para armazenar informações como configurações, dados de formulários e outros tipos de dados estruturados de forma fácil e legível.

## Como fazer:

Aqui estão alguns exemplos de como trabalhar com YAML usando Bash:

```
# Criar um arquivo YAML
cat <<EOF > my_file.yml
nome: João
sobrenome: Silva
idade: 30
EOF

# Ler e imprimir um valor de um arquivo YAML
nome=$(cat my_file.yml | grep "nome" | cut -d ":" -f 2)
echo "O nome é $nome"

# Alterar um valor em um arquivo YAML
sed -i 's/José/Maria/g' my_file.yml
```

### Saída de exemplo:

```
O nome é João

Arquivo my_file.yml depois da alteração:
nome: Maria
sobrenome: Silva
idade: 30
```

## Profundando:

O YAML (YAML Ain't Markup Language) foi criado em 2001 por Clark Evans e Ingy döt Net como uma alternativa mais simples ao formato de dados XML. Ele é baseado em indentação e espaços em branco, o que o torna fácil de ser lido e escrito por humanos. Além do Bash, o YAML também pode ser usado em outras linguagens como Python e Ruby.

## Veja também:

- [Documentação oficial do YAML](https://yaml.org/)
- [Tutorial de YAML para iniciantes](https://medium.com/swlh/a-beginners-guide-to-yaml-f9a85999b25d)
- [Comparação entre YAML e JSON](https://stackify.com/json-vs-yaml/)