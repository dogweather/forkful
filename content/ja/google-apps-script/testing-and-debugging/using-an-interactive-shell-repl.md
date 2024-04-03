---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:04:08.749100-07:00
description: "\u65B9\u6CD5: Google Apps Script\u306F\u3001Google\u88FD\u54C1\u9593\
  \u3067\u306E\u30BF\u30B9\u30AF\u3092\u81EA\u52D5\u5316\u3059\u308B\u305F\u3081\u306E\
  \u30AF\u30E9\u30A6\u30C9\u30D9\u30FC\u30B9\u306E\u30B9\u30AF\u30EA\u30D7\u30C8\u8A00\
  \u8A9E\u3067\u3059\u304C\u3001Python\u3084JavaScript\u306ENode.js\u306E\u3088\u3046\
  \u306A\u8A00\u8A9E\u306B\u3042\u308BREPL\u30C4\u30FC\u30EB\u3068\u540C\u69D8\u306E\
  \u5185\u8535\u30C4\u30FC\u30EB\u306F\u3042\u308A\u307E\u305B\u3093\u3002\u3057\u304B\
  \u3057\u3001Apps\u2026"
lastmod: '2024-03-13T22:44:41.447318-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script\u306F\u3001Google\u88FD\u54C1\u9593\u3067\u306E\u30BF\
  \u30B9\u30AF\u3092\u81EA\u52D5\u5316\u3059\u308B\u305F\u3081\u306E\u30AF\u30E9\u30A6\
  \u30C9\u30D9\u30FC\u30B9\u306E\u30B9\u30AF\u30EA\u30D7\u30C8\u8A00\u8A9E\u3067\u3059\
  \u304C\u3001Python\u3084JavaScript\u306ENode.js\u306E\u3088\u3046\u306A\u8A00\u8A9E\
  \u306B\u3042\u308BREPL\u30C4\u30FC\u30EB\u3068\u540C\u69D8\u306E\u5185\u8535\u30C4\
  \u30FC\u30EB\u306F\u3042\u308A\u307E\u305B\u3093\u3002\u3057\u304B\u3057\u3001Apps\
  \ Script\u30A8\u30C7\u30A3\u30BF\u306E\u30ED\u30B0\u8A18\u9332\u3068\u30C7\u30D0\
  \u30C3\u30B0\u6A5F\u80FD\u3092\u4F7F\u7528\u3059\u308B\u304B\u3001\u5916\u90E8\u74B0\
  \u5883\u3092\u8A2D\u5B9A\u3059\u308B\u3053\u3068\u3067\u3001\u985E\u4F3C\u3057\u305F\
  \u4F53\u9A13\u3092\u30B7\u30DF\u30E5\u30EC\u30FC\u30C8\u3059\u308B\u3053\u3068\u304C\
  \u3067\u304D\u307E\u3059\u3002\u3053\u3053\u3067\u306F\u3001Apps Script\u30A8\u30C7\
  \u30A3\u30BF\u5185\u3067 makeshift REPL\u3092\u4F5C\u6210\u3059\u308B\u3053\u3068\
  \u306B\u7126\u70B9\u3092\u5F53\u3066\u3066\u3044\u307E\u3059."
title: "\u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u30B7\u30A7\u30EB\uFF08REPL\uFF09\
  \u306E\u4F7F\u7528"
weight: 34
---

## 方法:
Google Apps Scriptは、Google製品間でのタスクを自動化するためのクラウドベースのスクリプト言語ですが、PythonやJavaScriptのNode.jsのような言語にあるREPLツールと同様の内蔵ツールはありません。しかし、Apps Scriptエディタのログ記録とデバッグ機能を使用するか、外部環境を設定することで、類似した体験をシミュレートすることができます。ここでは、Apps Scriptエディタ内で makeshift REPLを作成することに焦点を当てています。

1. **見慣れないREPL関数を作成する**:

```javascript
function myREPL() {
  var input = Logger.log('Enter your expression: ');
  try {
    var result = eval(input);
    Logger.log('Result: ' + result);
  } catch(e) {
    Logger.log('Error: ' + e.message);
  }
}
```

Apps Script環境では、従来のREPLと同様に直接ユーザー入力が実現可能ではないため、`input`変数を手動で変更し、式をテストするために`myREPL()`を実行します。

2. **サンプルコード実行**:

`2+2`を評価したいとします。その場合、`myREPL`関数を以下のように変更します：

```javascript
function myREPL() {
  var input = '2+2'; // ここに手動で式を入力
  // ほかの部分はそのまま...
}
```

`myREPL()`を実行した後、ログを確認します（表示 > ログ）。出力は以下のようになるはずです：

```
[20-xx-xxxx xx:xx:xx:xxx] Enter your expression:
[20-xx-xxxx xx:xx:xx:xxx] Result: 4
```

3. **Loggerを使用したデバッグ**:

より複雑なデバッグのために、`Logger.log(variable);`をコード内に挿入して変数の状態を印刷し、スクリプトの流れや中間状態を理解するのに役立ちます。

## 深堀り
REPLの概念は、インタラクティブセッションを可能にした1960年代のタイムシェアリングシステムから生まれた計算の歴史に深く根ざしています。Lispのような言語は、この環境で繁栄しました。REPLはその反復的な開発プロセスにとって重要でした。一方、Google Apps Scriptは、それよりずっと後に登場し、主にウェブのために設計されており、Googleスイート内のタスクを自動化することに焦点を当てており、イテレーティブなコンソールベースのプログラミングよりも重要視されています。

Google Apps Scriptは、そのクラウドベースの性質とウェブアプリのデプロイメントの焦点により、本来的にはリアルタイムの対話型コーディングセッションを箱から出してすぐにサポートしていません。その実行モデルは、ウェブイベント、タイムドリブンのトリガー、または環境内での手動の呼び出しなどによってトリガーされる関数を中心としており、REPLによって提供される即時のフィードバックループとは異なります。

Apps Scriptエディタ内の見慣れないREPLとデバッグ機能はある程度の対話性を提供しますが、多くのプログラミング言語で見られる伝統的なREPLの即時のフィードバックと効率性を完全には再現できません。Googleテクノロジーでより本格的なREPL体験を求める開発者は、GoogleのAPIを使用した外部JavaScript環境やNode.jsを探求することができます。これらは、より迅速で対話型のコーディングセッションを提供できますが、より多くの設定が必要となり、直接的なApps Script環境から一歩外に出ることが必要になるかもしれません。
