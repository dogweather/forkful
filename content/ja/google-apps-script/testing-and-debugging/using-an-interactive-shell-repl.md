---
title:                "インタラクティブシェル（REPL）の使用"
aliases:
- /ja/google-apps-script/using-an-interactive-shell-repl.md
date:                  2024-02-01T22:04:08.749100-07:00
model:                 gpt-4-0125-preview
simple_title:         "インタラクティブシェル（REPL）の使用"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/google-apps-script/using-an-interactive-shell-repl.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？

インタラクティブシェル、またはRead-Eval-Print Loop (REPL) は、単一のユーザー入力（式）を受け取り、評価し、結果をユーザーに返す、シンプルな対話型プログラミング環境です。プログラマーは、REPLを使用して、迅速なプロトタイピング、デバッグ、そしてプログラミング言語の構文や振る舞いを対話的に学ぶために利用します。

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
