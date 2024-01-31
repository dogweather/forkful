---
title:                "コマンドライン引数の読み取り"
date:                  2024-01-20T17:56:13.415352-07:00
model:                 gpt-4-1106-preview
simple_title:         "コマンドライン引数の読み取り"

category:             "Fish Shell"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
コマンドライン引数を読むとは、シェルスクリプトに渡されたパラメータを処理することです。プログラマーはこれを使って柔軟にスクリプトを制御し、異なるシナリオに対応します。

## How to: (方法)
```Fish Shell
# my_script.fish
for arg in $argv
    echo "引数: $arg"
end
```

実行例:

```Fish Shell
$ fish my_script.fish こんにちは 世界
引数: こんにちは
引数: 世界
```

## Deep Dive (深掘り)
Fish Shellでは、`$argv`はシェルスクリプトに渡された全ての引数を含むリストです。歴史的に、他のシェル(BashやZshなど)でも同様の概念がありますが、Fishはシンタックスが独特です。例えば、Bashでは`$@`や`$1`, `$2`のように引数を扱います。Fishでは、省略形やエラーハンドリングもより読みやすく書けます。例えば、何らかの引数が必要な場合、次のように書くこともできます。

```Fish Shell
if count $argv > /dev/null
    # 引数が一つ以上ある場合のコード
else
    echo "引数が必要です"
end
```

このようにFishでは、直感的で読みやすいコードが特徴です。スクリプト内で引数をチェックして分岐することは一般的なタスクで、Fishはそのための効果的なツールを提供します。

## See Also (参照)
- Fish公式ドキュメント - コマンドライン引数: https://fishshell.com/docs/current/index.html#variables-special
- チュートリアル - Fishでのスクリプトライティング: https://fishshell.com/docs/current/tutorial.html#tut_scripts
