---
title:                "コンピュータープログラミングにおける「テストの書き方」"
html_title:           "Go: コンピュータープログラミングにおける「テストの書き方」"
simple_title:         "コンピュータープログラミングにおける「テストの書き方」"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/writing-tests.md"
---

{{< edit_this_page >}}

# テストを書くとは？

プログラマーがコードを書く際に、テストを書いて実行することは重要です。テストは、バグを発見しやすくするために、実際のプログラムの機能や挙動を確認するものです。つまり、プログラムが想定通りに動作するかどうかを検証するためにテストを書きます。

## 方法：

Go言語を使って、テストを書く方法を見てみましょう。下のコードブロック内に、サンプルのコードとその実行結果を示します。

```
// コード例：

// テストする関数
func add(x, y int) int {
	return x + y
}

// テストコード
func TestAdd(t *testing.T) {
	result := add(2, 3)
	if result != 5 {
		t.Errorf("2 + 3 = %d; want 5", result)
	}
}

// 実行結果：
$ go test 
ok      _/home/test  0.006s
```

## ディープダイブ：

テストを書くことで、プログラムの品質をより高く保つことができます。以前は、手動でテストを行うことが一般的でしたが、現在では自動化されたテストが主流です。代表的なテストフレームワークとして、JUnitやSelenium等がありますが、Go言語にはネイティブなテストフレームワークであるtestingパッケージが用意されています。

## 参考：

関連する情報源は、以下のリンクから参照することができます。

- [Go言語 公式ドキュメント](https://golang.org/doc/)
- [ロバート・C・マーティンによる「Clean Code」](https://www.pearson.co.jp/book/9784822299972.html)
- [Tests in Go](https://medium.com/@teivah/tests-in-go-8d0709a4eea6)