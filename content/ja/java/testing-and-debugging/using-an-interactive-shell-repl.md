---
date: 2024-01-26 04:15:30.686721-07:00
description: "\u65B9\u6CD5\uFF1A Java 9\u3067\u5C0E\u5165\u3055\u308C\u305F`jshell`\u30C4\
  \u30FC\u30EB\u3092\u4F7F\u3048\u3070\u3001Java\u3067REPL\u3092\u958B\u59CB\u3059\
  \u308B\u3053\u3068\u306F\u7C21\u5358\u3067\u3059\u3002\u57FA\u672C\u30BB\u30C3\u30B7\
  \u30E7\u30F3\u3092\u958B\u59CB\u3059\u308B\u65B9\u6CD5\u306F\u6B21\u306E\u3068\u304A\
  \u308A\u3067\u3059\uFF1A."
lastmod: '2024-04-05T21:53:42.844869-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u30B7\u30A7\u30EB\uFF08REPL\uFF09\
  \u306E\u4F7F\u7528"
weight: 34
---

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
