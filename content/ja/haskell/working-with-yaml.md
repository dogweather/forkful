---
title:                "YAMLを扱う"
date:                  2024-01-19
html_title:           "Bash: YAMLを扱う"
simple_title:         "YAMLを扱う"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (何とその理由？)
YAMLは設定ファイルやデータの受け渡しに使われる。読みやすく、編集しやすいからプログラマーは好む。

## How to: (方法)
HaskellでYAMLを使うには、`yaml`ライブラリが便利です。こちらが基本的な例です。

```Haskell
import Data.Yaml
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
  yamlData <- BS.readFile "config.yaml"
  let parsedData = decodeEither' yamlData
  case parsedData of
    Left err -> print err
    Right config -> print (config :: Maybe Value)
```

`config.yaml` が

```yaml
name: Taro
age: 30
```

とすると、出力は以下の通り：

```Haskell
Just (Object (fromList [("name", String "Taro"), ("age", Number 30.0)]))
```

## Deep Dive (深堀り)
YAMLは2001年に登場。JSONやXMLの代替として使われることも。Haskellでは`yaml`パッケージが広く使われており、内部的には`libyaml`を利用してパフォーマンスを確保している。

## See Also (関連情報)
- YAML公式サイト: https://yaml.org
- Hackageのyamlパッケージ: https://hackage.haskell.org/package/yaml
- StackOverflow [Haskellタグ、YAMLタグあり]: https://stackoverflow.com/questions/tagged/haskell+yaml
