---
title:                "Trabalhando com yaml"
html_title:           "Haskell: Trabalhando com yaml"
simple_title:         "Trabalhando com yaml"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/working-with-yaml.md"
---

{{< edit_this_page >}}

# Programação em Haskell para Iniciantes: Trabalhando com YAML

## O que & por que?
Trabalhar com YAML é uma forma de armazenar e transmitir dados estruturados em um formato de texto fácil de ler e escrever. Programadores frequentemente usam YAML para armazenar configurações e informações de aplicativos, especialmente em sistemas web. É uma ferramenta útil para armazenar e transmitir informações de forma clara e concisa.

## Como fazer:
Segue abaixo algumas dicas básicas de como trabalhar com YAML em Haskell.

1. Primeiro, é necessário instalar a biblioteca `yaml` no seu projeto Haskell.
```Haskell
stack install yaml
```

2. Importe a biblioteca no seu código:
```Haskell
import Data.Yaml
```

3. Agora você pode ler um arquivo YAML utilizando a função `decodeFileMaybe`:
```Haskell
-- Lendo o arquivo "config.yaml" e armazenando em uma variável "dados"
dados <- decodeFileMaybe "config.yaml" :: IO (Maybe Object)
```

4. Para acessar os dados do arquivo YAML, você pode utilizar a função `lookup` combinada com o operador `>>=`:
```Haskell
-- Acessando os dados de uma chave específica
let porta = dados >>= lookup "porta" >>= decodeMaybe :: Maybe Int
```

5. Por fim, você pode utilizar os dados lidos para sua lógica de programação, por exemplo:
```Haskell
case porta of
  Just p -> putStrLn $ "A porta é " ++ show p
  Nothing -> putStrLn "Erro no arquivo YAML"
```

## Deep Dive:
O YAML foi criado em 2001 com o objetivo de oferecer uma alternativa mais legível ao formato XML. O nome é uma abreviação de "YAML Ain't Markup Language", destacando que não é uma linguagem de marcação, mas sim uma linguagem de serialização de dados.

Existem diversas alternativas ao YAML, como JSON e XML. No entanto, o YAML oferece uma sintaxe mais flexível e humana, sendo mais fácil de ler e escrever para humanos. Além disso, é suportado por diversas linguagens de programação, incluindo Haskell.

A implementação atual da biblioteca `yaml` para Haskell é baseada em um analisador LALR(1), o que permite uma análise mais eficiente e rápida de arquivos YAML.

## Veja também:
- [Documentação da biblioteca yaml](https://hackage.haskell.org/package/yaml)
- [Site oficial do YAML](https://yaml.org/)