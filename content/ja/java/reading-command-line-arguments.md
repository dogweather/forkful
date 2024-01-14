---
title:                "Java: コンピュータープログラミングにおける「コマンドライン引数の読み込み」"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## なぜ
コマンドライン引数を読み取るために読者が取り組む理由を1〜2文で説明します。

## 方法
```Java
public static void main(String[] args) {
    // コマンドライン引数の数を確認する
    System.out.println("コマンドライン引数の数: " + args.length);

    // コマンドライン引数をループして出力する
    for (int i = 0; i < args.length; i++) {
        System.out.println("コマンドライン引数 " + (i + 1) + ": " + args[i]);
    }
}
```

**出力例:**
```
コマンドライン引数の数: 3
コマンドライン引数 1: Hello
コマンドライン引数 2: World
コマンドライン引数 3: Java
```

## ディープダイブ
コマンドライン引数とは、プログラム起動時にコマンドラインから渡されるパラメータのことです。これを使用することで、実行時にプログラムの動作を制御したり、動的に変更したりすることができます。また、コマンドライン引数は複数渡すことができ、その数や内容に応じてプログラムの挙動を変えることもできます。

## その他の参考リンク
[コマンドライン引数を扱う方法](https://www.geeksforgeeks.org/command-line-arguments-in-java/)  
[Java プログラムの実行とコマンドライン引数の使用方法](https://codezine.jp/article/detail/2786)  
[実践 Java でコマンドライン引数を使ってみる](https://www.atmarkit.co.jp/ait/articles/1604/13/news034.html)