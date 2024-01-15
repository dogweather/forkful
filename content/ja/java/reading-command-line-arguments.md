---
title:                "コンピュータープログラミングの記事のタイトル：コマンドライン引数の読み取り"
html_title:           "Java: コンピュータープログラミングの記事のタイトル：コマンドライン引数の読み取り"
simple_title:         "コンピュータープログラミングの記事のタイトル：コマンドライン引数の読み取り"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## なぜ 
 
写真の撮影や音楽の制作など、多くの場面でコンピュータプログラムを使用しています。しかし、プログラムを実行する際に毎回同じ引数を入力するのは面倒です。そこで、コマンドライン引数を使うことで簡単にプログラムの動作を変えることができます。この記事では、コマンドライン引数の読み込み方を紹介します。 

## 使い方 

コマンドライン引数を読み込むには、 `main()` メソッドのパラメータとして `String` 型の配列 `args` を定義します。この配列には、プログラムを実行する際に入力した引数が格納されます。例えば、以下のコードを実行すると、 `args` 配列には `Hello, World!` という文字列が格納されます。

```Java
public static void main(String[] args) {
    System.out.println(args[0]);
}
```

コマンドラインから引数を入力して実行するには、`java` コマンドの後にプログラム名と引数をスペース区切りで入力します。例えば、以下のように入力して実行すると、コンソールには `Hello, World!` が出力されます。

```Java
java HelloWorld Hello, World!
```

また、複数の引数を入力することも可能です。この場合、 `args` 配列には入力順に値が格納されます。例えば、以下のように入力すると、コンソールには `Hello` と `World!` が順番に出力されます。

```Java
java HelloWorld Hello World!
```

## 詳細を深く掘り下げる 

コマンドライン引数には、プログラムの動作を変えるだけでなく、プログラム内で使う変数を動的に与えることもできます。例えば、以下のようなプログラムを作成したとします。

```Java
public static void main(String[] args) {
    // コマンドライン引数を整数型に変換して加算する
    int sum = Integer.parseInt(args[0]) + Integer.parseInt(args[1]);
    System.out.println(sum);
}
```

このプログラムは、コマンドライン引数として2つの整数を入力すると、その合計値を出力します。例えば、 `java Sum 5 10` と入力すると、コンソールには `15` が出力されます。このように、コマンドライン引数を使用することで、動的な値の処理が可能になります。 

## 関連リンク 

- [Java 公式チュートリアル - コマンドライン引数](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
- [JavaDoc - String 型の配列](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)