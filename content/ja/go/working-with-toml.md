---
title:                "TOMLを扱う方法"
date:                  2024-01-26T04:22:37.697021-07:00
model:                 gpt-4-0125-preview
simple_title:         "TOMLを扱う方法"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/working-with-toml.md"
---

{{< edit_this_page >}}

## 何となく何故か？
GoでTOMLを扱うというのは、TOML（Tom's Obvious, Minimal Language）ファイルを解析し、エンコードすることを含みます。プログラマーは、その読みやすさやデータ構造への簡単なマッピングのためにTOMLを選びます。これは設定ファイルにピッタリです。

## どうやって：
GoでTOMLを扱う場合、通常は`BurntSushi/toml`のようなライブラリを使用します。ここではTOML設定ファイルを解析する方法を簡単に見てみましょう：

```Go
package main

import (
    "fmt"
    "os"

    "github.com/BurntSushi/toml"
)

type Config struct {
    Title   string
    Owner   struct {
        Name string
    }
}

func main() {
    var config Config
    if _, err := toml.DecodeFile("config.toml", &config); err != nil {
        fmt.Println(err)
        return
    }
    fmt.Printf("Title: %s, Owner: %s\n", config.Title, config.Owner.Name)
}
```

`config.toml`のサンプル：

```Toml
title = "Example TOML"
[owner]
name = "Tom Preston-Werner"
```

サンプル出力：

```
Title: Example TOML, Owner: Tom Preston-Werner
```

## 深掘り
TOMLは、Tom Preston-Wernerによって2013年に紹介され、そのクリアなセマンティクスによって読みやすい最小限の設定ファイルフォーマットとして設計されました。Go開発者は、その直接性と複雑な階層を簡単に表現できる能力のため、JSONやYAMLなどの代替品よりも構成にTOMLをよく使用します。

YAMLには複雑な機能があり、潜在的なセキュリティ上の問題があるため、TOMLのフラットな設計は複雑さとタイプミスによるエラーを減らします。そして、JSONと違ってTOMLはコメントをサポートしているので、設定をインラインで説明するのが容易です。

GoでTOMLを扱う場合は、考慮すべきニュアンスがあります。構造体タグは、構造体がTOML構造にどのようにマップされるかをカスタマイズできますし、TOML配列とインラインテーブルがGoのスライスとマップにどのように解析されるかにも注意する必要があります。

## 参照
- TOML Specification：https://toml.io/en/
- BurntSushi/toml Library：https://github.com/BurntSushi/toml
- 設定ファイルフォーマットの比較：https://www.redhat.com/sysadmin/yaml-toml-json-differences
