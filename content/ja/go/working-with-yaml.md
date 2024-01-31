---
title:                "YAMLを扱う"
date:                  2024-01-19
html_title:           "Bash: YAMLを扱う"
simple_title:         "YAMLを扱う"

category:             "Go"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
## 何となぜ？

YAMLはデータ表現のフォーマット。設定ファイルやデータ転送で使われる。読みやすさと使い勝手で広く採用されている。

## How to:
## どうやって：

まず、`go-yaml`ライブラリを使う。これでYAMLを扱える。下記は読み込みと書き出しの例。

```Go
package main

import (
	"fmt"
	"log"

	"gopkg.in/yaml.v2"
)

// Config 構造体にYAMLのデータをマッピング
type Config struct {
	Database struct {
		User     string `yaml:"user"`
		Password string `yaml:"password"`
	} `yaml:"database"`
}

func main() {
	// YAMLデータ
	data := `
database:
  user: dbuser
  password: dbpass
`

	// Configオブジェクトを定義
	var config Config

	// YAMLデータをデコード
	err := yaml.Unmarshal([]byte(data), &config)
	if err != nil {
		log.Fatalf("error: %v", err)
	}
	fmt.Printf("--- config:\n%v\n\n", config)

	// ConfigオブジェクトをYAMLにエンコード
	d, err := yaml.Marshal(&config)
	if err != nil {
		log.Fatalf("error: %v", err)
	}
	fmt.Printf("--- yaml:\n%s\n\n", string(d))
}
```

出力：

```
--- config:
{Database:{User:dbuser Password:dbpass}}

--- yaml:
database:
  password: dbpass
  user: dbuser
```


## Deep Dive:
## ディープダイブ：

YAMLは"YAML Ain't Markup Language"の略。設定ファイルの簡単な作成を目指して2001年に開発された。JSONやXMLと比べ、人間に優しく、書きやすい。Goでは、`go-yaml`の他にも`ghodss/yaml`などのライブラリがある。パフォーマンスやAPIの違いを考慮して選ぶ。

## See Also:
## 関連情報：

- YAML公式サイト：https://yaml.org
- go-yamlライブラリ：https://github.com/go-yaml/yaml
- YAMLとJSONの比較：https://en.wikipedia.org/wiki/YAML#Comparison_with_JSON
