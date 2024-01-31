---
title:                "XMLの扱い方"
date:                  2024-01-26T04:31:54.487444-07:00
model:                 gpt-4-0125-preview
simple_title:         "XMLの扱い方"

category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/working-with-xml.md"
---

{{< edit_this_page >}}

## 何となぜ？
XMLを使用する作業は、コードを使用してXMLドキュメントを解析、作成、および操作することを含みます。プログラマーは、XMLの可読性と広範なサポートにより、構造化データにとって堅牢な選択肢であるため、データ交換、設定ファイル、およびWebサービスのためにこれを行います。

## 方法
Goでは、`encoding/xml`パッケージを使用します。XMLの解析と生成を行いましょう。
```go
package main

import (
	"encoding/xml"
	"fmt"
	"os"
)

// 構造体がXML要素にマップされます
type Plant struct {
	XMLName xml.Name `xml:"plant"`
	Id      int      `xml:"id,attr"`
	Name    string   `xml:"name"`
	Origin  []string `xml:"origin"`
}

func main() {
	coffee := &Plant{Id: 27, Name: "Coffee"}
	coffee.Origin = []string{"Ethiopia", "Brazil"}

	// 構造体をXMLにマーシャル
	output, err := xml.MarshalIndent(coffee, " ", "  ")
	if err != nil {
		fmt.Printf("Error: %v\n", err)
	}

	os.Stdout.Write([]byte(xml.Header))
	os.Stdout.Write(output)

	// XMLを構造体にアンマーシャル
	data := `
<plant id="27">
  <name>Coffee</name>
  <origin>Ethiopia</origin>
  <origin>Brazil</origin>
</plant>
`
	var p Plant
	if err := xml.Unmarshal([]byte(data), &p); err != nil {
		fmt.Printf("Error: %v", err)
		return
	}

	fmt.Printf("\n\nUnmarshaled: %+v", p)
}
```
サンプル出力：
```xml
<?xml version="1.0" encoding="UTF-8"?>
 <plant id="27">
   <name>Coffee</name>
   <origin>Ethiopia</origin>
   <origin>Brazil</origin>
 </plant>

Unmarshaled: {XMLName:{Space: Local:plant} Id:27 Name:Coffee Origin:[Ethiopia Brazil]}
```

## ディープダイブ
XMLは1990年代後半からあり、大規模な電子出版用に設計されましたが、すぐにWeb用に採用されました。JSONのような代替品が単純さを謳って登場しましたが、XMLのドキュメント検証スキーマと名前空間は、複雑なドキュメントに強力なままです。Goでは、`encoding/xml`がほとんどのタスクを処理しますが、巨大なドキュメントやストリーム処理の場合は、より低レベルの制御とパフォーマンスを向上させるために`xml.NewDecoder`および`xml.NewEncoder`を検討してください。

## 参照
- Goの`encoding/xml`パッケージ：https://pkg.go.dev/encoding/xml
- XMLチュートリアル：https://www.w3schools.com/xml/
- GoブログのXMLについて：https://blog.golang.org/xml
- JSONとXMLの比較：https://www.json.org/xml.html
