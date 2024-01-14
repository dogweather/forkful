---
title:    "Java: 「標準エラーに書き込む」"
keywords: ["Java"]
---

{{< edit_this_page >}}

「 ## なぜ」

なぜ：なぜ誰かが標準エラーへの書き込みに関わるのかを説明する1-2文のみ。

プログラミングをする上で、時には標準エラーにメッセージを書き込む必要があります。これは、プログラムでエラーが発生した場合に開発者にプログラムの状態を通知するためです。例えば、正しく動作しないコードを実行したり、入力ミスがあった場合などにエラーを書き込むことができます。

## 方法

まずは、Javaの ```System.err``` というメソッドを使って標準エラーに書き込む方法を紹介します。以下の例をご覧ください。

```Java
System.err.println("エラーが発生しました。");
```

このように、 ```.println()``` メソッドを使い、標準エラーに書き込むメッセージを指定します。出力結果は、コンソール上で赤い文字で表示されます。

また、```System.setErr()``` メソッドを使うことで、標準エラーの出力先を変更することもできます。例えば、ログファイルにエラーメッセージを書き込んでおくことができます。

```Java
System.setErr(new PrintStream("error.log"));
```

このように、指定したファイルにエラーメッセージを書き込むことができます。

## 詳細を調べる

標準エラーには、通常のエラーを書き込む```System.err```の他に、警告メッセージを書き込む```System.out```もあります。これらを上手く使い分けることで、プログラムの状態をより詳細に把握することができます。

また、エラーメッセージを翻訳することで、プログラムをより使いやすくすることができます。例えば、英語で書かれたエラーメッセージを日本語に変換することで、エンドユーザーにわかりやすいメッセージを表示することができます。

## 参考リンク

- [Java: StandardErrorクラス](https://docs.oracle.com/javase/jp/8/docs/api/java/lang/System.html)
- [Java: PrintStreamクラス](https://docs.oracle.com/javase/jp/8/docs/api/java/io/PrintStream.html)
- [プログラミングで使う標準出力と標準エラーの意味と使い方](https://qiita.com/mima_ita/items/1e5c5c8614b6a394555e) 

## 参照

リンク：
- [Java: StandardError class](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html)
- [Java: PrintStream class](https://docs.oracle.com/javase/8/docs/api/java/io/PrintStream.html)
- [Meanings and usage of standard output and standard error in programming](https://qiita.com/mima_ita/items/1e5c5c8614b6a394555e)