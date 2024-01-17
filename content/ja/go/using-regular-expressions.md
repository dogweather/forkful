---
title:                "正規表現の利用"
html_title:           "Go: 正規表現の利用"
simple_title:         "正規表現の利用"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 何となぜ？

正規表現を使うとは、コンピュータ言語におけるパターンマッチングの手法です。プログラマーはこれを使うことで、文字列やテキストデータから特定のパターンを抽出したり、置換したりすることができます。

## 方法：

Go言語では、正規表現パターンを表す ```regexp``` パッケージを使います。下記の例では、指定した文字列に「こんにちは」という単語が含まれているかどうかをチェックして、結果を返します。

```Go
#include <regexp>

func main() {
  input := "こんにちは、世界！"
  re := regexp.MustCompile("こんにちは")
  match := re.MatchString(input)

  if match {
    fmt.Println("指定した文字列に「こんにちは」という単語が含まれています")
  } else {
    fmt.Println("指定した文字列に「こんにちは」という単語が含まれていません")
  }

}
```

出力は以下のようになります。

```
指定した文字列に「こんにちは」という単語が含まれています
```

## 深く掘り下げる

正規表現は、1960年代にケン・トンプソンが開発したもので、今でも広く使われています。Go言語以外にも、PerlやPythonなどの言語でもサポートされています。

正規表現では、パイプ記号(|)やワイルドカード(*)など、特殊文字を使ってパターンを指定します。また、より細かい検索や変換をするために、文字列やグループをキャプチャすることもできます。

## 関連情報を参照する

Go言語で正規表現を使う方法については、公式ドキュメントを参照することができます。また、レギュラーテーブルを生成するツールとして、「RegExr」や「RegexBuddy」などがあります。