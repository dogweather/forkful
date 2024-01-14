---
title:    "Java: 「スタンダードエラーへの書き込み」"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# なぜ標準エラー出力を書くのですか？

プログラミングの世界では、標準エラー出力は重要な役割を果たします。エラーメッセージを出力することによって、開発者は問題を特定し、修正することができます。そのため、標準エラー出力を書くことは、プログラムの実行やデバッグにとって大切なスキルです。

# どうやって標準エラー出力を書くのですか？

Javaでは、Systemクラスを使用して標準エラー出力を行うことができます。以下のコード例を参考にしてください。

```Java
public class HelloWorld {

  public static void main(String[] args) {
    System.err.println("Hello, world!"); // 標準エラー出力を行う
  }
}
```

上記のコードを実行すると、"Hello, world!"というエラーメッセージがコンソールに出力されます。

# 標準エラー出力についての詳細

標準エラー出力は、System.errオブジェクトを使用して行われます。このオブジェクトは、System.outと同じように使用することができますが、エラーメッセージを出力するために特別に設計されています。通常、標準エラー出力は赤色のフォントで表示され、コンソールの標準出力とは別のストリームで処理されます。

# 参考サイト

- Javaの公式ドキュメント: https://docs.oracle.com/javase/jp/8/docs/api/java/lang/System.html
- 標準エラー出力についての詳細な説明: https://www.baeldung.com/java-stdout-stderr
- Javaのプログラミング入門サイト: https://java-beginner.com/java/how-to-write-standard-error/
- 日本語のJavaコミュニティサイト: https://www.java-users.jp/java/introduction/how-to-use-system_err_println
- Javaのコンソール出力に関するチュートリアル: https://www.javabrahman.com/corejava/java-console-output-made-simple/ 

# 関連記事を見る

- Javaの標準入力と標準出力について: https://qiita.com/ir_fuma/items/dec40458477d105f7598
- Javaのエラー処理についての基本的な知識: https://timeshift-blog.azurewebsites.net/java-error-handling/
- Javaでコンソールアプリケーションを作成する方法: https://meltybitsblog.com/java-console/