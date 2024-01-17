---
title:                "標準エラーに書き込む"
html_title:           "Java: 標準エラーに書き込む"
simple_title:         "標準エラーに書き込む"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 何をするのか？なぜするのか？

標準エラーに書き込むとは、プログラマーがコードの実行中に出力するエラーメッセージを指定することです。これは、プログラムが予期しない問題に遭遇した場合に、開発者にコードの修正が必要であることを知らせるために使用されます。

## 方法：

以下のようにJavaのコードブロックに示す例を使用して、標準エラーに書き込む方法を説明します。

```
// テスト用のエラーメッセージを作成する
String errorMessage = "エラー：このコードは間違っています。";

// 標準エラーにエラーメッセージを出力する
System.err.println(errorMessage);
```

上記のコードを実行すると、次のような出力結果が得られます。

`エラー：このコードは間違っています。`

## 詳細情報：

### 歴史的な背景：

標準エラーへの書き込みは、プログラミング言語の歴史とともに発展した機能の一つです。以前のプログラミング言語では、エラーの処理はプログラム内で手動で行われていましたが、近年の言語では標準エラーへの書き込みをサポートするようになっています。

### 代替手段：

標準エラーへの書き込み以外にも、プログラマーはエラーメッセージをファイルや他の出力ストリームに書き込むこともできます。ただし、標準エラーへの書き込みはプログラマーにとって便利であり、メッセージを見つけやすくするために特別なカラーやフォーマットを使用することもできます。

### 実装の詳細：

Javaでは、標準エラーへの書き込みには```System.err.println()```メソッドを使用します。このメソッドは、標準エラーで使用される```PrintStream```クラスのインスタンスに対して動作します。

## 関連情報：

- Javaプログラミング言語ガイド：http://www.oracle.com/technetwork/java/javase/overview/index.html
- 標準エラーについて：https://docs.oracle.com/javase/tutorial/essential/io/cl.html