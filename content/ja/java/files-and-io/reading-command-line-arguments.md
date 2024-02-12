---
title:                "コマンドライン引数の読み取り"
aliases:
- ja/java/reading-command-line-arguments.md
date:                  2024-01-20T17:56:16.599495-07:00
model:                 gpt-4-1106-preview
simple_title:         "コマンドライン引数の読み取り"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
コマンドライン引数を読むこととは、プログラム実行時にユーザーからのパラメータを受け取ることです。これにより、柔軟性とユーザーカスタマイズ性をプログラムに提供できるため、プログラマーはこの機能を利用します。

## How to: (方法)
```Java
public class CommandLineExample {
    public static void main(String[] args) {
        if (args.length > 0) {
            System.out.println("引数が指定されています！");
            for (String arg : args) {
                System.out.println("引数: " + arg);
            }
        } else {
            System.out.println("引数がありません。");
        }
    }
}
```
コマンドラインから実行:
```
$ javac CommandLineExample.java
$ java CommandLineExample これはテストです。
引数が指定されています！
引数: これは
引数: テストです。
```

## Deep Dive (深掘り)
コマンドライン引数はUNIXやDOSの時代から使われている古典的な手法です。Javaでは`String[] args`を`main`メソッドのパラメータとして使うことでアクセスします。代替手段としてはプロパティファイル、環境変数、GUI要素の利用などがありますが、初期設定や環境特有の値には引数が便利です。引数の数、順序、そして型は実施する処理によって決まり、必要に応じて変換・検証を行うべきです。

## See Also (関連情報)
- [Oracle Java Documentation - Command-Line Arguments](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
- [Apache Commons CLI](https://commons.apache.org/proper/commons-cli/): コマンドライン引数を解析するためのライブラリ
- [JArgs](http://jargs.sourceforge.net/): コマンドラインオプションパーサーのためのGNU GPLライブラリ
