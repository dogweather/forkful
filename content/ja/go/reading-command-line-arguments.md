---
title:    "Go: コマンドライン引数の読み込み"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# なぜ
コマンドライン引数を読み取る必要があるのかを説明します。

コマンドライン引数を読み取ることで、コマンドラインからプログラムの動作を制御したり、特定のデータを渡したりすることができます。また、プログラムの実行時に引数を指定することで、柔軟にプログラムの挙動を変更することができます。

## 使い方

コマンドライン引数を読み取るには、標準ライブラリの`flag`パッケージを使用します。以下のようなコードを書くことで、コマンドライン引数を読み取ることができます。

```Go
import "flag"

// フラグ変数を定義する
var flagVar string

// フラグ変数とデフォルト値を指定する
flag.StringVar(&flagVar, "flagVar", "default", "description")

// フラグをパースする
flag.Parse()

// フラグ変数を使用する
fmt.Println(flagVar)
```

上記の例では、`flagVar`という名前のフラグ変数を定義し、そのデフォルト値を`default`として指定しています。また、フラグには`-flagVar`という名前と、説明文を付けています。プログラムを実行する際には、`-flagVar`というオプションを指定することで、フラグ変数の値を変更することができます。

以下のように実行すると、`flagVar`の値が`changed`になります。

```bash
go run main.go -flagVar changed
```

## ディープダイブ

コマンドライン引数を読み取る場合、様々なデータ型を扱うことができます。文字列や数値だけでなく、真偽値や配列なども指定することができます。また、任意の数のフラグを受け付けることも可能です。

さらに、デフォルト値や説明文を指定するだけでなく、フラグに対して引数の取得方法をカスタマイズすることもできます。`flag`パッケージには様々なメソッドが用意されているので、ぜひ詳しく調べてみてください。

## 関連リンク

- `flag`パッケージのドキュメント：https://golang.org/pkg/flag/
- フラグの使い方についての解説記事：https://gobyexample.com/command-line-flags
- コマンドライン引数を扱う実践的なアドバイス：https://peter.bourgon.org/blog/2017/06/09/the-xflag-package.html