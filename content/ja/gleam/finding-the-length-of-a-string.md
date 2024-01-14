---
title:    "Gleam: 文字列の長さを見つける"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Why (なぜ): 
文字列の長さを求める作業には、多くの理由があります。たとえば、プログラミングの中で、文字列を取り扱う必要がある場合や、文字列を処理する際には、その長さを知る必要があります。また、文字列の長さを求めることによって、プログラムの実行速度を最適化することもできます。Gleamを使うと、文字列の長さを簡単に求めることができます。

## How To (方法):
まずは、Gleamで文字列を定義してみましょう。以下のコードブロックを使うと、"Hello, World!"という文字列を定義できます。 

```Gleam
let string = "Hello, World!"
```

次に、`String.length()`関数を使って、指定した文字列の長さを調べることができます。例えば、上で定義した"Hello, World!"の長さは12ですので、以下のようにコードを書くことができます。

```Gleam
let length = String.length(string)
```

出力結果は、`length`変数に格納されていますので、以下のように表示することができます。

```Gleam
IO.println("Length of string is #{length}")
```

実行結果は、"Length of string is 12"と表示されます。

## Deep Dive (詳細):
Gleamでは、文字列の長さを求めるのに`String.length()`関数を使いますが、実際にどのように働いているのか知りたい方もいるかもしれません。この関数は、文字列が持つ`size`というメソッドを呼び出しています。このメソッドは、文字列のバイト数を返します。つまり、文字列の長さは、含まれている文字の数とバイト数が一致するわけではありません。例えば、日本語の文字はUTF-8エンコーディングが使われるため、1文字あたり3バイトとなります。

## See Also ( 関連リンク ):
- [Gleamの公式ドキュメント](https://gleam.run/documentation/)
- [GleamのGithubリポジトリ](https://github.com/gleam-lang/gleam)