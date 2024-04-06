---
date: 2024-01-26 04:14:17.046743-07:00
description: "\u4F7F\u3044\u65B9\uFF1A Fish\u3067\u306F\u3001\u30A4\u30F3\u30BF\u30E9\
  \u30AF\u30C6\u30A3\u30D6\u30B7\u30A7\u30EB\u304C\u8D77\u52D5\u6642\u306E\u30C7\u30D5\
  \u30A9\u30EB\u30C8\u30E2\u30FC\u30C9\u3067\u3059\u3002\u5B9F\u969B\u306E\u52D5\u4F5C\
  \u306F\u4EE5\u4E0B\u306E\u3088\u3046\u306B\u306A\u308A\u307E\u3059\uFF1A."
lastmod: '2024-04-05T21:53:43.521572-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u30B7\u30A7\u30EB\uFF08REPL\uFF09\
  \u306E\u4F7F\u7528"
weight: 34
---

## 使い方：
Fishでは、インタラクティブシェルが起動時のデフォルトモードです。実際の動作は以下のようになります：

```Fish Shell
> set color blue
> echo "The sky is $color"
空は青い
```

組み込み関数の実行やコマンド置換で遊ぶこともできます：

```Fish Shell
> function cheer
      echo "Go Fish $argv!"
  end
> cheer Coders
Go Fish Coders!
```

関数を定義するだけでなく、その場でコードスニペットを実行し、即座に出力を見ることができます：

```Fish Shell
> math "40 / 2"
20
```

## 詳細な解説
REPLの概念は、1960年代のLispプログラミング言語にまで遡ります。この形式のインタラクティブなプログラミングは、Pythonの`ipython`やRubyの`irb`のような環境のベンチマークを設定しました。Fishはユーザーフレンドリーさとインタラクティブな使用に焦点を当てて、このトレンドを継続しています。

Fishは、Bashのような他のシェルとは異なり、最初からインタラクティビティを念頭に設計されています。構文ハイライト、自動提案、タブ補完を提供し、REPLスタイルのワークフローでの使用を強力にします。さらに良いことに、あなたのコマンドは記憶され、検索可能であり、繰り返しのテストを容易にします。

FishのREPLに代わるものには、`bash-completion`や`oh-my-zsh`のような拡張機能とともに使用される`bash`や`zsh`がありますが、Fishは箱から出してすぐにより豊かな体験を提供しがちです。

## 参照：
- Fishドキュメント：https://fishshell.com/docs/current/index.html
- Fishと他のシェルの興味深い比較：https://www.slant.co/versus/2209/3686/~fish_vs_bash
- REPLについてのさらなる深掘り：https://en.wikipedia.org/wiki/Read–eval–print_loop
- Lispにおけるインタラクティブプログラミング、歴史的な視点：http://www.paulgraham.com/ilisp.html
