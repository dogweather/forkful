---
date: 2024-01-26 04:14:17.046743-07:00
description: "REPL\u3001\u3059\u306A\u308F\u3061Read-Eval-Print\u2026"
lastmod: '2024-03-13T22:44:42.738216-06:00'
model: gpt-4-0125-preview
summary: "REPL\u3001\u3059\u306A\u308F\u3061Read-Eval-Print Loop\u306F\u3001\u5358\
  \u4E00\u306E\u30E6\u30FC\u30B6\u30FC\u5165\u529B\u3092\u53D7\u3051\u53D6\u308A\u3001\
  \u305D\u308C\u3092\u5B9F\u884C\u3057\u3001\u7D50\u679C\u3092\u8FD4\u3059\u5BFE\u8A71\
  \u5F0F\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u74B0\u5883\u3067\u3059\u3002\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30B3\u30F3\u30D1\u30A4\u30EB\u3084\u5B8C\
  \u5168\u306A\u30D7\u30ED\u30B0\u30E9\u30E0\u306E\u5B9F\u884C\u306E\u30AA\u30FC\u30D0\
  \u30FC\u30D8\u30C3\u30C9\u306A\u3057\u306B\u3001\u5373\u6642\u306E\u30D5\u30A3\u30FC\
  \u30C9\u30D0\u30C3\u30AF\u3001\u30C7\u30D0\u30C3\u30B0\u3001\u305D\u3057\u3066\u30B3\
  \u30FC\u30C7\u30A3\u30F3\u30B0\u6982\u5FF5\u306E\u8FC5\u901F\u306A\u5B9F\u9A13\u306E\
  \u305F\u3081\u306B\u3053\u308C\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002."
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
