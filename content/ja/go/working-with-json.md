---
title:                "JSONを扱う方法"
html_title:           "Arduino: JSONを扱う方法"
simple_title:         "JSONを扱う方法"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
## 何となぜ？

JSONはデータ交換フォーマットです。シンプルで読みやすく、多言語間のデータ通信に使われます。

## How to:
## 方法：

Goでは`encoding/json`パッケージを用いてJSONとのやり取りを行います。基本的な使い方を見てみましょう。

```Go
package main

import (
    "encoding/json"
    "fmt"
    "log"
)

type User struct {
    ID   int    `json:"id"`
    Name string `json:"name"`
}

func main() {
    // JSON文字列
    jsonString := `{"id": 1, "name": "Taro"}`
    
    // JSONを構造体にデコード
    var user User
    err := json.Unmarshal([]byte(jsonString), &user)
    if err != nil {
        log.Fatal(err)
    }
    fmt.Printf("%+v\n", user) // 出力：{ID:1 Name:Taro}

    // 構造体をJSONにエンコード
    newJson, err := json.Marshal(user)
    if err != nil {
        log.Fatal(err)
    }
    fmt.Println(string(newJson)) // 出力：{"id":1,"name":"Taro"}
}
```

## Deep Dive
## 詳細：

JSONはJavaScript Object Notationの略で、1999年にECMAScript言語仕様とともに登場しました。XMLなどの代替品もありますが、JSONはその軽量さから特にWeb APIとの連携で広く使われています。Goでは`encoding/json`パッケージが内部リフレクションを使っており、構造体タグを通してフィールド名のカスタマイズができます。

## See Also
## 関連情報：

- GoのJSONパッケージ公式ドキュメント: https://pkg.go.dev/encoding/json
- Go言語によるJSON処理の詳しいチュートリアル: https://blog.golang.org/json
- JSONとは: https://www.json.org/json-ja.html