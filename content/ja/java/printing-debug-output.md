---
title:    "Java: デバッグ出力の出力"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/java/printing-debug-output.md"
---

{{< edit_this_page >}}

## なぜデバッグ出力を表示するのか

デバッグ出力を表示することには、多くの利点があります。例えば、プログラムがどのように実行されているかを確認したり、エラーを特定したりするのに役立ちます。また、特定の変数の値を確認することで、プログラムの動作を理解することができます。

## 方法

プログラム内でデバッグ出力を表示するには、```System.out.println()```メソッドを使用します。これは文字列や変数の値をコンソールに表示させることができます。例えば、以下のコードを実行すると、```Hello World```という文字列がコンソールに表示されます。

```Java
public class HelloWorld {
    public static void main(String[] args) {
        System.out.println("Hello World");
    }
}
```

変数の値を表示する場合は、```System.out.println()```メソッドの中に変数名を記述します。例えば、以下のコードでは```num```という変数の値がコンソールに表示されます。

```Java
public class Example {
    public static void main(String[] args) {
        int num = 10;
        System.out.println(num);
    }
}
```

## ディープダイブ

デバッグ出力を使うと、プログラムの実行経路や変数の値を詳しく確認することができます。さらに、```System.out.println()```メソッドを使わずに、```System.out.print()```メソッドを使用することで、表示される出力が改行されないため、大量のデータを正確に確認することができます。

## 参考リンク

- [Java 公式ドキュメント](https://docs.oracle.com/en/java/)
- [Java デバッグ入門 - TechAcademyマガジン](https://techacademy.jp/magazine/8661)
- [Java でコーディングする際に Debug 出力/StackTrace を print する方法のまとめ - Qiita](https://qiita.com/nagase/items/6cbe95e667a3b98f2968)
- [Java でデバッグを行う方法 概要 - programmer-office.com](https://www.programmer-office.com/coding-javadebug/)