---
title:                "Trabalhando com YAML"
date:                  2024-01-19
html_title:           "Arduino: Trabalhando com YAML"
simple_title:         "Trabalhando com YAML"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/working-with-yaml.md"
---

{{< edit_this_page >}}

## O Que é & Porquê?
YAML, que significa "YAML Ain't Markup Language" (recursivamente), é um formato de serialização de dados legível por humanos frequentemente usado para configurações de aplicativos e troca de dados entre linguagens. Programadores utilizam YAML pela sua clareza e simplicidade, facilitando a escrita e leitura de dados sem sacrificar a potência.

## Como Fazer:
Para processar YAML em Haskell, você pode usar a biblioteca `yaml`. Primeiro, instale com `cabal install yaml`. Agora, vamos parsear um YAML simples:

```Haskell
import Data.Yaml

main :: IO ()
main = do
  let yamlData = "nome: João Silva\nidade: 30\nlinguagens:\n  - Haskell\n  - Elm"
  let parsed = decodeEither' yamlData :: Either ParseException Value
  print(parsed)
```

Saída de exemplo:

```
Right (Object (fromList [("nome",String "João Silva"),("idade",Number 30.0),("linguagens",Array [String "Haskell",String "Elm"])]))
```

## Aprofundamento:
YAML foi criado em 2001 e sua especificação foi inspirada em linguagens como XML, C, Python e Perl. Enquanto JSON é uma alternativa popular devido à sua compatibilidade com JavaScript, YAML permite comentários e é menos verboso, tornando-o preferível para configurações. Ao trabalhar com Haskell, a implementação com a biblioteca `yaml` é direta: ela usa as funções encode e decode para serializar e deserializar dados, respectivamente.

## Veja Também:
- Documentação oficial do YAML: [YAML Specification](https://yaml.org/spec/1.2/spec.html)
- Repositório da biblioteca `yaml` para Haskell: [yaml on Hackage](https://hackage.haskell.org/package/yaml)
