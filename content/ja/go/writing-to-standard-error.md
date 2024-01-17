---
title:                "標準エラーへの書き込み"
html_title:           "Go: 標準エラーへの書き込み"
simple_title:         "標準エラーへの書き込み"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 何＆なぜ？
標準エラーへの書き込みとは何かを説明するために、プログラマーがそれを行う理由を説明します。標準エラーに書き込むことによって、プログラマーはプログラムの正常および異常な出力を区別することができます。

## 方法：
標準エラーへの書き込みは、Go言語では ```os.Stderr``` という変数を使用して行われます。次のコードは、"Hello, error!"というメッセージを標準エラーに書き込む例です。

```Go
fmt.Fprintln(os.Stderr, "Hello, error!")
```

出力：

```
Hello, error!
```

## 深く掘り下げる：
標準エラーへの書き込みは、プログラムのデバッグやエラー処理において重要です。以前のプログラミング言語では、コンソールログに書き込んでいましたが、Go言語では標準エラーを使用することが推奨されています。

標準エラーへの書き込み以外の方法としては、標準出力への書き込みやログファイルへの書き込みがあります。また、標準エラーへ書き込むことによって、他のプロセスやデーモンからの入力を取得することもできます。

```os.Stderr``` の実装については、Go言語のソースコードを参照してください。

## 関連情報：
- 標準エラーについてのGo言語ドキュメント：https://golang.org/pkg/os/#Stderr
- 標準エラーと標準出力の違いについて：https://stackoverflow.com/questions/23392736/what-is-the-difference-between-stdout-and-stderr-in-go
- 標準エラーへの書き込みの実装方法：https://github.com/golang/go/blob/master/src/os/signal_unix.go#L61