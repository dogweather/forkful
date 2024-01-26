---
title:                "インタラクティブシェル（REPL）の使用"
date:                  2024-01-26T04:15:30.686721-07:00
model:                 gpt-4-0125-preview
simple_title:         "インタラクティブシェル（REPL）の使用"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## 何となぜ？
REPL（Read-Eval-Print Loop）は、単一のユーザー入力を処理し、コードを実行して結果を返すインタラクティブシェルです。プログラマーは、即時フィードバックと反復が可能なため、短い実験、デバッグや学習に使用します。

## 方法：
Java 9で導入された`jshell`ツールを使えば、JavaでREPLを開始することは簡単です。基本セッションを開始する方法は次のとおりです：

```Java
jshell> int sum(int a, int b) {
   ...> return a + b;
   ...> }
|  sum(int,int)メソッドが作成されました

jshell> sum(5, 7)
$1 ==> 12
```

いつでも`/exit`で終了します。

```Java
jshell> /exit
|  さようなら
```

## ディープダイブ
`jshell`の前には、JavaプログラマーにはPythonやRubyの開発者のような公式のREPLがありませんでした。彼らはIDEを使用したり、些細なタスクでもフルプログラムを書いていました。Java 9の`jshell`はゲームチェンジャーであり、そのギャップを埋めました。

代替手段にはオンラインコンパイラーやIDEプラグインがありますが、`jshell`の即時性には及びません。内部に関しては、`jshell`はJava Compiler APIを使用してコード断片を実行します。これはかなり素晴らしいことです。それは遊び場以上のものであり、ライブラリのインポート、クラスの定義などが可能です。これにより、プロトタイピングに向いている強力なツールとなります。

## 関連項目
- [JShellユーザーガイド](https://docs.oracle.com/javase/9/jshell/introduction-jshell.htm)
- [Javaプラットフォーム、スタンダードエディションツールリファレンス](https://docs.oracle.com/javase/9/tools/tools-and-command-reference.htm#JSWOR719)
- [JavaコンパイラAPI](https://docs.oracle.com/javase/9/docs/api/javax/tools/JavaCompiler.html)