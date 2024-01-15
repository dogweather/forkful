---
title:                "JSONを使ったプログラミング作業"
html_title:           "Go: JSONを使ったプログラミング作業"
simple_title:         "JSONを使ったプログラミング作業"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/working-with-json.md"
---

{{< edit_this_page >}}

## なぜ
JSONの使用における*なぜ*を最大2文で説明します。

JSONは軽量で柔軟なデータ形式であり、モダンなウェブ開発やAPI通信などでよく使われるため、Go言語をマスターしたいプログラマーにとって重要なスキルです。

## 使い方
```Go
import (
	"encoding/json"
	"fmt"
)

type Person struct {
	Name  string
	Age   int
	City  string
	Hobby string
}

func main() {
	// Create a Person struct instance
	p := Person{
		Name:  "John",
		Age:   30,
		City:  "New York",
		Hobby: "Playing guitar",
	}

	// Convert struct to JSON
	encoded, err := json.Marshal(p)
	if err != nil {
		fmt.Println(err)
		return
	}
	fmt.Println(string(encoded)) // Output: {"Name":"John","Age":30,"City":"New York","Hobby":"Playing guitar"}

	// Convert JSON back to struct
	var decoded Person
	err = json.Unmarshal(encoded, &decoded)
	if err != nil {
		fmt.Println(err)
		return
	}
	fmt.Println(decoded) // Output: {John  30 New York Playing guitar}
}
```

## ディープダイブ
JSONはJavaScript Object Notationの略で、軽量なデータ交換フォーマットとして1980年代から使用されてきました。Go言語では、標準パッケージの`encoding/json`を使用して簡単にJSONを扱うことができます。また、JSONの特徴として、人間が読み書きしやすいテキスト形式であること、配列やオブジェクトを含む複雑なデータ構造にも対応できることが挙げられます。

## 関連情報
[Go言語公式ドキュメント - encoding/jsonパッケージ](https://golang.org/pkg/encoding/json/)