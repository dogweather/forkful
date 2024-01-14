---
title:                "Java: コンピュータープログラミングの記事のタイトル「コマンドライン引数の読み取り」"
simple_title:         "コンピュータープログラミングの記事のタイトル「コマンドライン引数の読み取り」"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# なぜコマンドライン引数を読むのか

プログラミングに慣れていない人にとって、コマンドライン引数を使用するのはやや難しいかもしれません。しかし、それでもコマンドライン引数を使うメリットは多くあります。例えば、コマンドライン引数を使用することで、プログラムを実行する際に追加の情報を渡すことができます。これにより、同じプログラムを再利用することができますし、プログラムの動作をカスタマイズすることも可能になります。この記事では、コマンドライン引数の読み方と使用方法を説明します。

## コマンドライン引数の読み方

コマンドライン引数を読むには、Javaプログラムで提供されている `args` 配列を使用します。この配列には、プログラムを実行する際に指定した引数が格納されています。以下のコード例を見てみましょう。このプログラムは、コマンドライン引数として与えられた数値の合計を求めるものです。

```java
public class CommandLineArgsExample {
  public static void main(String[] args) {
    int sum = 0;
    for (String arg : args) {
      int num = Integer.parseInt(arg); // 引数を数値に変換
      sum += num; // 合計に追加
    }
    System.out.println("Total: " + sum); // 合計を出力
  }
}
```

プログラムを実行する際に、コマンドライン引数として数値を与えてみましょう。例えば、`java CommandLineArgsExample 1 2 3` のように実行すると、以下のような出力が得られます。

```
Total: 6
```

## コマンドライン引数の詳細

コマンドライン引数を読む方法がわかったところで、もう少し深く見てみましょう。コマンドライン引数を読むことで、プログラムをより柔軟にすることができます。例えば、同じプログラムを実行する際に、引数を変えることで異なる動作を実現することができます。また、プログラムを実行する前に、コマンドライン引数を表示してユーザーに入力を促すこともできます。

さらに、コマンドライン引数はオプションとしても使用することができます。例えば、`java MyClass -d` のように、引数の前に `-` をつけてオプションを指定することができます。プログラムの実行方法を指定するオプションは、プログラムのデバッグモードに入るためのものや、出力結果をファイルに保存するためのものなど、さまざまな用途に利用されます。

# See Also

- [Javaコマンドライン引数の使い方 | インストール方法.com](https://installhowto.com/java/command-line-arguments/)
- [Javaオプション引数の指定方法 | TechScore Academy](https://www.techscore.com/blog/2013/01/11/javacommandlineandoption/)