---
title:                "YAMLを扱う"
html_title:           "Bash: YAMLを扱う"
simple_title:         "YAMLを扱う"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (なにとなぜ？)
YAMLは設定ファイルやデータ交換に使うシンプルな形式です。可読性が高く、人間にも機械にも理解しやすいため、広くプログラマーに使われています。

## How to: (どうやって？)
Gleamには標準でYAMLを解析するライブラリがないので、外部クレートを使います。以下はGleamでYAMLを扱う簡単な例です。

```gleam
// 外部クレートを使ってYAMLをパースする
// 注: 実際のGleamコードとして実行可能なものではない
import yaml_crate

pub fn parse_yaml(yaml_str: String) -> Result(any, String) {
    yaml_crate.parse(yaml_str)
}

fn main() {
    let yaml_data = "
    age: 35
    name: John Doe
    children:
      - name: Jane Doe
        age: 15
    "

    let result = parse_yaml(yaml_data)
    case result {
        Ok(value) -> value
        Error(e) -> e
    }
}
```

このコードは、YAML文字列を解析し、結果またはエラーメッセージを出力します。

## Deep Dive (深い潜水)
YAML (YAML Ain't Markup Language)は、JSONのより読みやすい代替として2001年に登場しました。JSONやXMLと比べて、スペースでストラクチャを表現し、繰り返しを避けるアンカーやエイリアスが使えます。Gleamにおいては、RustやElixirのライブラリをバインディングするか、もしくは独自のパーサを実装することが可能です。

## See Also (関連情報)
- YAML公式サイト: https://yaml.org
- YAML仕様: https://yaml.org/spec/1.2/spec.html
- JSONとYAMLの比較: https://www.json2yaml.com/
- Gleamの外部クレート: https://hex.pm/packages?search=gleam
