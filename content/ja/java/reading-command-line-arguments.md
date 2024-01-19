---
title:                "コマンドライン引数の読み取り"
html_title:           "Bash: コマンドライン引数の読み取り"
simple_title:         "コマンドライン引数の読み取り"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 何となぜ？

コマンドライン引数の読み取りは、ユーザーがプログラムを実行する際にサプライするパラメータをプログラムが読み取ることです。これにより、プログラム의動作を柔軟に制御することができます。

## どうやって：

Javaのコマンドライン引数を読み取る方法を示すコードです。

```Java
public class Main {
    public static void main(String[] args) {
      for (String s: args) {
         System.out.println(s);
      }
    }
}
```

出力例：

```bash
> java Main これはテストです
これはテストです
```

## ディープダイブ

コマンドライン引数の読み取りはUnixの時代から存在しており、ユーザーがプログラムの振る舞いを制御できるようにする重要な手段となっています。Javaでは、`main`関数に渡される`String`型配列を通じてコマンドライン引数にアクセスします。

代替手段としては、設定ファイルの読み取りやユーザーインターフェースからの入力などがありますが、コマンドライン引数の方がシンプルで迅速にパラメータを渡すことができます。

Javaでは、コマンドライン引数は`main`メソッドに`String`配列として渡されます。この配列は0インデックスでアクセス可能で、コマンドラインから渡されたそれぞれの引数を保持します。

## 参考文献

- OracleのJavaチュートリアル: コマンドライン引数 [リンク](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
- Stack Overflow: Javaでのコマンドライン引数の使用 [リンク](https://stackoverflow.com/questions/890966/what-is-string-args-parameter-in-main-method-java)