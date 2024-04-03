---
date: 2024-01-26 04:15:30.686721-07:00
description: "REPL\uFF08Read-Eval-Print Loop\uFF09\u306F\u3001\u5358\u4E00\u306E\u30E6\
  \u30FC\u30B6\u30FC\u5165\u529B\u3092\u51E6\u7406\u3057\u3001\u30B3\u30FC\u30C9\u3092\
  \u5B9F\u884C\u3057\u3066\u7D50\u679C\u3092\u8FD4\u3059\u30A4\u30F3\u30BF\u30E9\u30AF\
  \u30C6\u30A3\u30D6\u30B7\u30A7\u30EB\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306F\u3001\u5373\u6642\u30D5\u30A3\u30FC\u30C9\u30D0\u30C3\u30AF\u3068\u53CD\
  \u5FA9\u304C\u53EF\u80FD\u306A\u305F\u3081\u3001\u77ED\u3044\u5B9F\u9A13\u3001\u30C7\
  \u30D0\u30C3\u30B0\u3084\u5B66\u7FD2\u306B\u4F7F\u7528\u3057\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:41.949528-06:00'
model: gpt-4-0125-preview
summary: "REPL\uFF08Read-Eval-Print Loop\uFF09\u306F\u3001\u5358\u4E00\u306E\u30E6\
  \u30FC\u30B6\u30FC\u5165\u529B\u3092\u51E6\u7406\u3057\u3001\u30B3\u30FC\u30C9\u3092\
  \u5B9F\u884C\u3057\u3066\u7D50\u679C\u3092\u8FD4\u3059\u30A4\u30F3\u30BF\u30E9\u30AF\
  \u30C6\u30A3\u30D6\u30B7\u30A7\u30EB\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306F\u3001\u5373\u6642\u30D5\u30A3\u30FC\u30C9\u30D0\u30C3\u30AF\u3068\u53CD\
  \u5FA9\u304C\u53EF\u80FD\u306A\u305F\u3081\u3001\u77ED\u3044\u5B9F\u9A13\u3001\u30C7\
  \u30D0\u30C3\u30B0\u3084\u5B66\u7FD2\u306B\u4F7F\u7528\u3057\u307E\u3059\u3002."
title: "\u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u30B7\u30A7\u30EB\uFF08REPL\uFF09\
  \u306E\u4F7F\u7528"
weight: 34
---

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
