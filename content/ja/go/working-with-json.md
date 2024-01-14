---
title:                "Go: JSONの操作"
simple_title:         "JSONの操作"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/working-with-json.md"
---

{{< edit_this_page >}}

## なぜJSONを使うのか

JSONは、データを構造化して扱うための重要なフォーマットです。そのため、Go言語を使ってプログラミングをする際には、JSONを扱うことが頻繁にあります。例えば、APIのリクエストやレスポンス、データベースからのデータの取得など、さまざまな場面でJSONを利用することができます。

## JSONの扱い方

Go言語では、標準ライブラリの"encoding/json"パッケージを使用して、JSONを扱うことができます。"encoding/json"パッケージには、JSONをGoのデータ構造に変換するための関数や、逆にGoのデータ構造からJSONに変換するための関数が用意されています。

例えば、下記のようにJSONデータをGoのデータ構造に変換することができます。

```Go
package main

import (
	"encoding/json"
	"fmt"
)

type Person struct {
	Name string `json:"name"`
	Age  int    `json:"age"`
}

func main() {
	jsonData := []byte(`{"name": "Yamada Taro", "age": 26}`)
	var person Person
	json.Unmarshal(jsonData, &person)
	fmt.Println(person) // {Yamada Taro 26}
}
```

そして、Goのデータ構造をJSONに変換する場合は、下記のように行います。

```Go
package main

import (
	"encoding/json"
	"fmt"
)

type Person struct {
	Name string `json:"name"`
	Age  int    `json:"age"`
}

func main() {
	person := Person{
		Name: "Suzuki Hanako",
		Age:  32,
	}
	jsonData, _ := json.Marshal(person)
	fmt.Println(string(jsonData)) // {"name":"Suzuki Hanako","age":32}
}
```

## JSONの深い掘り下げ

JSONには、配列やネストしたオブジェクトなど、さまざまな構造が存在します。そのため、Go言語においても、そのような複雑なJSONデータを扱う際には、より複雑な処理が必要になります。

例えば、下記のようなJSONデータを扱う場合は、配列の中にネストしたオブジェクトがあるため、より複雑な処理が必要になります。

```json
{
	"people": [
		{"name": "Yamada Taro", "age": 26},
		{"name": "Suzuki Hanako", "age": 32}
	]
}
```

また、JSONには、オブジェクトに名前がない配列や、名前が重複するオブジェクトなどもあります。Go言語では、"encoding/json"パッケージのタグを使って、そのようなJSONデータをGoのデータ構造に変換することができます。

## また見る

- [GolangでのJSONの扱い方](https://qiita.com/Sekky0905/items/eee12d8e247166ef5875)
- [Golangの標準ライブラリencoding/jsonの使い方](https://qiita.com/seihmd/items/ab3845eeb8adb60f2dcc)
- [Go言語でのJSONタグの使い方](https://kakkoyakakko2.hatenablog.com/entry/2018/06/21/002215)