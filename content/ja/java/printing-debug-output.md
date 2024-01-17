---
title:                "デバッグ出力の表示"
html_title:           "Java: デバッグ出力の表示"
simple_title:         "デバッグ出力の表示"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/printing-debug-output.md"
---

{{< edit_this_page >}}

## 何が& なぜ？

デバッグ出力をプリントするとは何か？プログラマーがそれをする理由は？という疑問を抱いている方もいるかもしれませんね。簡単に説明すると、デバッグ出力とは、コード実行中にプログラム内部の情報を確認するためにコンソールやログファイルに出力することです。プログラムのバグを見つけるために、またはプログラムが正しく動いていることを確認するために、デバッグ出力を使用するのです。

## 方法：

下の ```Java ... ``` コードブロックに示すように、デバッグ出力をプリントする方法はとても簡単です。まずは、 ```System.out.println()``` を使用してコンソール上にメッセージを出力してみましょう。次に、 ```System.out.print()``` を使用すると、改行なしでコンソール上にメッセージを出力することができます。また、 ```System.out.printf()``` を使用すると、特定のフォーマットに従ってデータを出力することができます。例えば、数値を小数点以下2桁まで出力したい場合は、 ```System.out.printf("%.2f", 3.1415)``` のように書くことができます。

```Java
System.out.println("Hello World!");
System.out.print("This is a ");
System.out.print("print statement.");
System.out.printf("%.2f", 3.1415);
```

```
Hello World!
This is a print statement.
3.14
```

## さらに詳しく：

デバッグ出力を使用することで、プログラムのバグを見つけたり、問題を解決したりすることができますが、他にも代替手段があります。例えば、デバッガーを使用することで、プログラム実行中に変数の値やステップ実行ができるため、より精密なデバッグが可能です。また、ログレベルを設定することで、異なる詳細度のデバッグ出力を出力することができます。

実際のプログラム内でデバッグ出力を使用する際には、注意点もあります。デバッグ出力を残したままコードをリリースすると、パフォーマンスの低下やセキュリティ上のリスクが発生する可能性があるため、プロダクション環境ではデバッグ出力を削除することが重要です。

## 関連情報：

- [Javaの標準出力（System.out）の使い方](https://www.javadrive.jp/start/printoutput/index1.html)
- [Debuting 101: 日本語でデバッグ出力の考え方](https://capytech.com/blogs/2018-11-24-debuting-101-jp/)