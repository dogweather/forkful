---
title:                "Trabalhando com yaml"
html_title:           "Fish Shell: Trabalhando com yaml"
simple_title:         "Trabalhando com yaml"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/working-with-yaml.md"
---

{{< edit_this_page >}}

## O que é e porquê?

Trabalhar com YAML é algo que muitos programadores fazem em seus projetos. Esse formato de dados é bastante utilizado para armazenar e organizar informações em um formato legível para humanos e máquinas. YAML é especialmente útil em projetos de configuração e implantação de software, pois permite que os desenvolvedores definam variáveis e propriedades de forma clara e concisa. 

## Como fazer:

Para trabalhar com YAML no Fish Shell, é necessário primeiro instalar a extensão `fish-yaml`. Você pode fazer isso usando o gerenciador de pacotes `fisher`. Depois, basta importar o módulo `yaml` em seus scripts e aplicar as funções disponíveis. Aqui está um exemplo básico de como ler um arquivo YAML e imprimir seu conteúdo:

```fish
fisher install jorgebucaran/fish-yaml # instalar a extensão fish-yaml
source ~/.config/fish/functions/yaml.fish # importar o módulo yaml
set doc (yaml -l ./config.yaml) # ler o arquivo yaml e armazenar na variável doc
echo $doc # imprimir o conteúdo do arquivo
```

A saída será o conteúdo do arquivo YAML, que pode ser usado para definir variáveis ou executar outras ações em seus scripts.

## Profundidade do Mergulho:

O YAML foi lançado em 2001 como uma maneira simples de representar dados em formato de grafo. Ele foi criado em resposta ao perda de popularidade do formato XML, que era mais verboso e difícil de ler. Além disso, YAML é compatível com várias linguagens de programação, tornando-o uma escolha popular para projetos em equipe. 

Embora o Fish Shell tenha suporte nativo para leitura e escrita de arquivos YAML, também existem outras opções, como os módulos `yaml2l`, `yamlexport`, e `darjeeling`. Cada um tem suas próprias vantagens e desvantagens, então é importante experimentá-los e escolher o que melhor se adapta às suas necessidades.

Ao trabalhar com YAML no Fish Shell, é importante lembrar que nem todas as funcionalidades disponíveis em outras linguagens de programação serão suportadas. É sempre uma boa prática testar seus scripts e verificar a documentação oficial para garantir que você está usando a sintaxe correta e aproveitando ao máximo as funcionalidades disponíveis.

## Veja também:

- [Site oficial do YAML](https://yaml.org/)
- [Página do módulo yaml no GitHub](https://github.com/jorgebucaran/fish-yaml)
- [Documentação oficial do Fish Shell](https://fishshell.com/docs/current/)