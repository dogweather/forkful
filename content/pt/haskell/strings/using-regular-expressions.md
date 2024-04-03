---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:57.090413-07:00
description: "Express\xF5es regulares na programa\xE7\xE3o s\xE3o sequ\xEAncias de\
  \ caracteres que definem um padr\xE3o de busca, tipicamente empregadas para pesquisa\
  \ e manipula\xE7\xE3o de\u2026"
lastmod: '2024-03-13T22:44:46.615597-06:00'
model: gpt-4-0125-preview
summary: "Express\xF5es regulares na programa\xE7\xE3o s\xE3o sequ\xEAncias de caracteres\
  \ que definem um padr\xE3o de busca, tipicamente empregadas para pesquisa e manipula\xE7\
  \xE3o de strings."
title: "Usando express\xF5es regulares"
weight: 11
---

## Como:
Em Haskell, as funcionalidades de regex não fazem parte da biblioteca padrão, necessitando do uso de pacotes de terceiros como `regex-base` junto com um backend compatível como `regex-posix` (para suporte a regex POSIX), `regex-pcre` (para regex compatível com Perl), etc. Veja como você pode usar esses pacotes para trabalhar com expressões regulares.

Primeiro, garanta que você tenha os pacotes instalados adicionando `regex-posix` ou `regex-pcre` ao arquivo `.cabal` do seu projeto ou instalando diretamente via cabal:

```bash
cabal install regex-posix
```
ou
```bash
cabal install regex-pcre
```

### Usando `regex-posix`:
```haskell
import Text.Regex.Posix ((=~))

-- Verifica se uma string corresponde a um padrão
isMatch :: String -> String -> Bool
isMatch text pattern = text =~ pattern :: Bool

-- Encontra a primeira correspondência
findFirst :: String -> String -> String
findFirst text pattern = text =~ pattern :: String

main :: IO ()
main = do
    print $ isMatch "hello world" "wo"
    -- Saída: True
    print $ findFirst "good morning, good night" "good"
    -- Saída: "good"
```

### Usando `regex-pcre`:
```haskell
import Text.Regex.PCRE ((=~))

-- Encontra todas as correspondências
findAll :: String -> String -> [String]
findAll text pattern = text =~ pattern :: [String]

main :: IO ()
main = do
    print $ findAll "test1 test2 test3" "\\btest[0-9]\\b"
    -- Saída: ["test1","test2","test3"]
```

Cada biblioteca tem suas particularidades, mas a metodologia geral de usar `=~` para aplicar a regex permanece consistente, seja verificando por uma correspondência ou extraindo substrings. Escolher entre `regex-posix` ou `regex-pcre` depende em grande parte das necessidades do seu projeto e das capacidades específicas de regex requeridas.
