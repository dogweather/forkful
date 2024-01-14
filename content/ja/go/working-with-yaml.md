---
title:                "Go: 「YAMLを使ったプログラミング」"
simple_title:         "「YAMLを使ったプログラミング」"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/working-with-yaml.md"
---

{{< edit_this_page >}}

## なぜ？

YAMLとは何か、それを使用する理由は？
YAMLとは、構造化されたデータを表現するためのファイル形式です。Go言語では、YAMLファイルを簡単に読み書きできるライブラリが存在します。この記事では、そのライブラリを使用してYAMLファイルを操作する方法を紹介します。

## 方法

まず、YAMLファイルを読み込む方法から説明します。まずは、`import`文を使って`gopkg.in/yaml.v2`をインポートします。次に、`Unmarshal`関数を使用して、YAMLファイルをGoのデータ構造に変換します。例えば、以下のようなYAMLファイルがあったとします。

```YAML
name: John
age: 30
hobby:
- cooking
- reading
```

このファイルを読み込むGoのコードは以下のようになります。

```Go
package main

import (
	"fmt"
	"gopkg.in/yaml.v2"
	"io/ioutil"
)

type Person struct {
	Name  string
	Age   int
	Hobby []string
}

func main() {
	file, err := ioutil.ReadFile("person.yml")
	if err != nil {
		panic(err)
	}

	var p Person
	yaml.Unmarshal(file, &p)
	fmt.Println(p.Name)
	fmt.Println(p.Age)
	fmt.Println(p.Hobby)
}
```

このコードを実行すると、`John`、`30`、`[cooking reading]`という結果が出力されます。

次に、YAMLファイルを書き込む方法を紹介します。`Marshal`関数を使用して、Goのデータ構造をYAML形式に変換します。以下のようなGoの構造体があったとします。

```Go
type Animal struct {
	Name string `yaml:"name"`
	Type string `yaml:"type"`
}
```

この構造体をYAMLファイルに書き込むコードは以下のようになります。

```Go
func main() {
	a := Animal{Name: "Pochi", Type: "dog"}
	y, err := yaml.Marshal(a)
	if err != nil {
		panic(err)
	}
	ioutil.WriteFile("animal.yml", y, 0644)
}
```

`animal.yml`という名前のファイルが生成され、以下のような内容になります。

```YAML
name: Pochi
type: dog
```

## 深堀り

`gopkg.in/yaml.v2`パッケージにはさまざまな機能があります。例えば、`Marshal`関数を使わずに、直接Goのデータ構造をYAMLファイルに書き込む方法や、YAMLファイルの一部を更新する方法などがあります。また、YAMLファイルを検証するためのツールも提供されています。

さらに、YAMLファイルをブラウザ上で編集できるライブラリや、Goの構造体から自動的にYAMLファイルを生成するツールなどもあります。それらを組み合わせることで、より簡単にYAMLファイルを扱うことができます。

## 参考リンク

- [ソースコード](https://github.com/go-yaml/yaml)
- [ドキュメント](https://gopkg.in/yaml.v2)
- [YAMLとは](https://yaml.org/)
- [YAMLファイルを読み書きする方法](https://www.callicoder.com/working-with-yaml-in-go/)
- [YAMLをWeb上で編集するライブラリ](https://github.com/sahilm/yamled)
- [