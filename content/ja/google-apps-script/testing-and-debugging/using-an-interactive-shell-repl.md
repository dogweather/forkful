---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:04:08.749100-07:00
description: "\u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u30B7\u30A7\u30EB\u3001\
  \u307E\u305F\u306FRead-Eval-Print Loop (REPL)\u2026"
lastmod: '2024-03-13T22:44:41.447318-06:00'
model: gpt-4-0125-preview
summary: "\u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u30B7\u30A7\u30EB\u3001\
  \u307E\u305F\u306FRead-Eval-Print Loop (REPL) \u306F\u3001\u5358\u4E00\u306E\u30E6\
  \u30FC\u30B6\u30FC\u5165\u529B\uFF08\u5F0F\uFF09\u3092\u53D7\u3051\u53D6\u308A\u3001\
  \u8A55\u4FA1\u3057\u3001\u7D50\u679C\u3092\u30E6\u30FC\u30B6\u30FC\u306B\u8FD4\u3059\
  \u3001\u30B7\u30F3\u30D7\u30EB\u306A\u5BFE\u8A71\u578B\u30D7\u30ED\u30B0\u30E9\u30DF\
  \u30F3\u30B0\u74B0\u5883\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\
  \u3001REPL\u3092\u4F7F\u7528\u3057\u3066\u3001\u8FC5\u901F\u306A\u30D7\u30ED\u30C8\
  \u30BF\u30A4\u30D4\u30F3\u30B0\u3001\u30C7\u30D0\u30C3\u30B0\u3001\u305D\u3057\u3066\
  \u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u8A00\u8A9E\u306E\u69CB\u6587\u3084\u632F\
  \u308B\u821E\u3044\u3092\u5BFE\u8A71\u7684\u306B\u5B66\u3076\u305F\u3081\u306B\u5229\
  \u7528\u3057\u307E\u3059\u3002."
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
