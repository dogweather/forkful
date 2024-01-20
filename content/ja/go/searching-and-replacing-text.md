---
title:                "テキストの検索と置換"
html_title:           "Java: テキストの検索と置換"
simple_title:         "テキストの検索と置換"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 何となぜ？

テキストの検索と置換は、特定の文字列を見つけてそのマッチとなる部分を新しいテキストで置き換えるプロセスです。プログラマーは新規機能を追加するため、エラーを修正するため、またはコードを最適化するためにしばしばこれを行います。

## 方法

以下に、Goでテキストの検索と置換を行う簡単な例を示します。

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	str := "Hello, world!"
	newStr := strings.Replace(str, "world", "Go", -1)

	fmt.Println(newStr)
}
```

上記のコードを実行すると、以下の出力が表示されます。

```
Hello, Go!
```

`strings.Replace` 機能は、指定した文字列（この場合は "world"）を新しい文字列（この場合は "Go"）で置き換えます。

## より深く

テキストの検索と置換は、最初のプログラミング言語が開発されたころから存在します。これはコード管理と更新を簡単にし、重要なエラーを早期に見つけ出すための助けになります。

置換機能は多くの代替案と共に存在します。例えば正規表現を使用する replacer function の設定も可能です。

`strings.Replace`は、源の各インデックスに対してパターンが一致するかどうかを確認しながら linear scan を使用して実装されています。置き換えるテキストが見つける文字列よりも長い場合、Goは新しいバッファーを作成し、そこに結果をビルドします。

## 関連情報

- Go公式ドキュメンテーション: [strings package](https://golang.org/pkg/strings/)
- Learn Go: [String Manipulation](https://www.learn-go.dev/topics/string-manipulation)
- Go playground: 練習や実験のための [サンドボックス環境](https://play.golang.org/)