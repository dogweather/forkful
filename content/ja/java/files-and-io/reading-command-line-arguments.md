---
date: 2024-01-20 17:56:16.599495-07:00
description: "How to: (\u65B9\u6CD5) \u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\
  \u6570\u306FUNIX\u3084DOS\u306E\u6642\u4EE3\u304B\u3089\u4F7F\u308F\u308C\u3066\u3044\
  \u308B\u53E4\u5178\u7684\u306A\u624B\u6CD5\u3067\u3059\u3002Java\u3067\u306F`String[]\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:50:55.912717-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) \u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u306F\
  UNIX\u3084DOS\u306E\u6642\u4EE3\u304B\u3089\u4F7F\u308F\u308C\u3066\u3044\u308B\u53E4\
  \u5178\u7684\u306A\u624B\u6CD5\u3067\u3059\u3002Java\u3067\u306F`String[] args`\u3092\
  `main`\u30E1\u30BD\u30C3\u30C9\u306E\u30D1\u30E9\u30E1\u30FC\u30BF\u3068\u3057\u3066\
  \u4F7F\u3046\u3053\u3068\u3067\u30A2\u30AF\u30BB\u30B9\u3057\u307E\u3059\u3002\u4EE3\
  \u66FF\u624B\u6BB5\u3068\u3057\u3066\u306F\u30D7\u30ED\u30D1\u30C6\u30A3\u30D5\u30A1\
  \u30A4\u30EB\u3001\u74B0\u5883\u5909\u6570\u3001GUI\u8981\u7D20\u306E\u5229\u7528\
  \u306A\u3069\u304C\u3042\u308A\u307E\u3059\u304C\u3001\u521D\u671F\u8A2D\u5B9A\u3084\
  \u74B0\u5883\u7279\u6709\u306E\u5024\u306B\u306F\u5F15\u6570\u304C\u4FBF\u5229\u3067\
  \u3059\u3002\u5F15\u6570\u306E\u6570\u3001\u9806\u5E8F\u3001\u305D\u3057\u3066\u578B\
  \u306F\u5B9F\u65BD\u3059\u308B\u51E6\u7406\u306B\u3088\u3063\u3066\u6C7A\u307E\u308A\
  \u3001\u5FC5\u8981\u306B\u5FDC\u3058\u3066\u5909\u63DB\u30FB\u691C\u8A3C\u3092\u884C\
  \u3046\u3079\u304D\u3067\u3059\u3002"
title: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u306E\u8AAD\u307F\u53D6\
  \u308A"
weight: 23
---

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
