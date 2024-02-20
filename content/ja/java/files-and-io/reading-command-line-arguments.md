---
date: 2024-01-20 17:56:16.599495-07:00
description: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u3092\u8AAD\u3080\
  \u3053\u3068\u3068\u306F\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\u5B9F\u884C\u6642\u306B\
  \u30E6\u30FC\u30B6\u30FC\u304B\u3089\u306E\u30D1\u30E9\u30E1\u30FC\u30BF\u3092\u53D7\
  \u3051\u53D6\u308B\u3053\u3068\u3067\u3059\u3002\u3053\u308C\u306B\u3088\u308A\u3001\
  \u67D4\u8EDF\u6027\u3068\u30E6\u30FC\u30B6\u30FC\u30AB\u30B9\u30BF\u30DE\u30A4\u30BA\
  \u6027\u3092\u30D7\u30ED\u30B0\u30E9\u30E0\u306B\u63D0\u4F9B\u3067\u304D\u308B\u305F\
  \u3081\u3001\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3053\u306E\u6A5F\u80FD\u3092\
  \u5229\u7528\u3057\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:01.128880
model: gpt-4-1106-preview
summary: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u3092\u8AAD\u3080\
  \u3053\u3068\u3068\u306F\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\u5B9F\u884C\u6642\u306B\
  \u30E6\u30FC\u30B6\u30FC\u304B\u3089\u306E\u30D1\u30E9\u30E1\u30FC\u30BF\u3092\u53D7\
  \u3051\u53D6\u308B\u3053\u3068\u3067\u3059\u3002\u3053\u308C\u306B\u3088\u308A\u3001\
  \u67D4\u8EDF\u6027\u3068\u30E6\u30FC\u30B6\u30FC\u30AB\u30B9\u30BF\u30DE\u30A4\u30BA\
  \u6027\u3092\u30D7\u30ED\u30B0\u30E9\u30E0\u306B\u63D0\u4F9B\u3067\u304D\u308B\u305F\
  \u3081\u3001\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3053\u306E\u6A5F\u80FD\u3092\
  \u5229\u7528\u3057\u307E\u3059\u3002"
title: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u306E\u8AAD\u307F\u53D6\
  \u308A"
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
