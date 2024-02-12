---
title:                "インタラクティブシェル（REPL）の使用"
aliases: - /ja/fish-shell/using-an-interactive-shell-repl.md
date:                  2024-01-26T04:14:17.046743-07:00
model:                 gpt-4-0125-preview
simple_title:         "インタラクティブシェル（REPL）の使用"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## 何となぜ？
REPL、すなわちRead-Eval-Print Loopは、単一のユーザー入力を受け取り、それを実行し、結果を返す対話式プログラミング環境です。プログラマーは、コンパイルや完全なプログラムの実行のオーバーヘッドなしに、即時のフィードバック、デバッグ、そしてコーディング概念の迅速な実験のためにこれを使用します。

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
