---
date: 2024-01-27 16:21:00.793885-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:42.849260-06:00'
model: gpt-4-0125-preview
summary: "\u2026"
title: "CLI\u30EF\u30F3\u30E9\u30A4\u30CA\u30FC\u3067\u306E\u30D5\u30A1\u30A4\u30EB\
  \u306E\u30A4\u30F3\u30D7\u30EC\u30FC\u30B9\u7DE8\u96C6"
---

{{< edit_this_page >}}

## 何となぜ？

RubyでCLI（コマンドラインインターフェース）のワンライナーを使用してファイルをその場で編集することは、エディターでファイルを開き、変更を加え、それを保存する必要なく、ターミナルから直接ファイルを変更できることを意味します。このテクニックは、簡単な修正、バッチ更新、または繰り返し作業の自動化に非常に有用であり、時間と労力の両方を節約します。

## 方法：

Rubyは、コマンドラインからその場でファイルを編集する簡単な方法を提供しています。Rubyの`-i`スイッチを使用すると、提供されたファイルに直接作用するようRubyに指示できます。実際の例でこれがどのように機能するか見てみましょう。次の内容を持つ`greetings.txt`というファイルがあるとします。

```
こんにちは、世界！
こんにちは、Ruby！
こんにちは、プログラミング！
```

そして、「こんにちは」を「Hi」に置き換えたいとしましょう。以下のように行います。

```Ruby
ruby -i -pe "gsub(/Hello/, 'Hi')" greetings.txt
```

このコマンドを実行した後、`greetings.txt`は次のように更新されます。

```
Hi, world!
Hi, Ruby!
Hi, programming!
```

データを台無しにする可能性が心配な場合、Rubyはバックアップを作成することでこれをカバーしています。例えば：

```Ruby
ruby -i.bak -pe "gsub(/Hello/, 'Bye')" greetings.txt
```

これで、編集された`greetings.txt`と共に、同じディレクトリに元の内容を持つ`greetings.txt.bak`が見つかります。

## 詳細解説

Rubyのその場でのファイル編集の魔法は、Perlのようなテキスト処理能力とRubyの独自の文法的エレガンスの組み合わせから生まれます。歴史的に、Perlは特にテキスト操作のためのクイックなワンライナースクリプティングに最適な言語でした。Rubyはこのパラダイムを採用し、強力なコマンドラインスクリプティング機能を提供しています。

その場での編集には、Perl自体やUnixシステムのストリームエディタであるsedなど、他の言語での代替手段が存在します。それぞれには強みがあります—Perlはテキスト処理能力で知られ、sedはストリーム編集タスクのシンプルさで比類がありません。しかし、Rubyはバランスを提供し、特にRubyにすでに慣れ親しんでいる人にとって、より読みやすくユーザーフレンドリーな構文で強力なテキスト操作を提供します。

実装面では、Rubyのその場での編集は、元のファイルの名前を変更し、元のファイル名で新しいファイルを作成し、その新しいファイルに変更を書き込みながら元のファイルから読み取ることで機能します。このアプローチは、操作の原子性を確保します。つまり、ファイル全体が正常に処理されるか、変更が加えられないかのどちらかであり、編集プロセス中にデータの完全性を保護します。このメカニズムは、電力故障やプロセスの強制終了などの中断に対しても、Rubyの例外処理と組み合わせて、少なくともバックアップが無事であることを確実にします。

要約すると、Rubyのその場でのファイル編集は、コマンドラインから直接テキスト操作タスクのための強力さ、シンプルさ、そしてエレガンスのブレンドを提供する、そのユーティリティとしてのスクリプティング言語の証です。
