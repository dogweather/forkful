---
title:    "Java: 「コマンドライン引数の読み取り」"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# なぜ

コマンドライン引数の読み込みについて学ぶことは、Javaプログラミングで重要なスキルです。それを理解することは、より効率的なコードを書くためや、プログラムの柔軟性を高めるために必要です。

# 使い方

コマンドライン引数を読み込むには、```args```配列を使用します。最初の要素は、プログラム名を示し、2番目以降の要素はコマンドライン引数が格納されます。例えば、```java ProgramName arg1 arg2```の場合、```args[0]```は"ProgramName"、```args[1]```は"arg1"、```args[2]```は"arg2"となります。以下のコードブロックは、```PrintArgs```というクラスの例です。

```Java
public class PrintArgs {
  public static void main(String[] args) {
    // 最初の要素（プログラム名）を表示
    System.out.println("プログラム名: " + args[0]);
    
    // コマンドライン引数を全て表示
    for (int i = 1; i < args.length; i++) {
      System.out.println("引数" + i + ": " + args[i]);
    }
  }
}
```

以下は、上記のコードを実行した際の出力例です。

```
プログラム名: java
引数1: arg1
引数2: arg2
```

# 詳細を掘り下げる

コマンドライン引数は、プログラムの実行時に動的に与えられる値です。これにより、同じプログラムでも異なる引数を与えることで、異なる結果を得ることができます。また、コマンドラインから入力を受け付けることで、ユーザーとの対話的なプログラムを作ることができます。

さらに、```args```配列以外にも、```System.getProperty()```メソッドを使用することで、システムプロパティや環境変数を取得することもできます。

# 参考リンク

- [Javaコマンドライン引数 (JavaBeat)](https://www.javabeat.net/java-command-line-args/)
- [Java I/O - コマンドラインからの入出力 (TutorialsTeacher)](https://www.tutorialsteacher.com/java/cmd-command-prompt-in-java)
- [Java SystemクラスのgetProperty()メソッドの使い方 (JavaCodeExamples)](https://www.javacodeexamples.com/java-system-getproperty-method-example/1223)