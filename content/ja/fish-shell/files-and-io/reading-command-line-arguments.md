---
date: 2024-01-20 17:56:13.415352-07:00
description: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u3092\u8AAD\u3080\
  \u3068\u306F\u3001\u30B7\u30A7\u30EB\u30B9\u30AF\u30EA\u30D7\u30C8\u306B\u6E21\u3055\
  \u308C\u305F\u30D1\u30E9\u30E1\u30FC\u30BF\u3092\u51E6\u7406\u3059\u308B\u3053\u3068\
  \u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3053\u308C\u3092\u4F7F\
  \u3063\u3066\u67D4\u8EDF\u306B\u30B9\u30AF\u30EA\u30D7\u30C8\u3092\u5236\u5FA1\u3057\
  \u3001\u7570\u306A\u308B\u30B7\u30CA\u30EA\u30AA\u306B\u5BFE\u5FDC\u3057\u307E\u3059\
  \u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.758070-06:00'
model: gpt-4-1106-preview
summary: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u3092\u8AAD\u3080\
  \u3068\u306F\u3001\u30B7\u30A7\u30EB\u30B9\u30AF\u30EA\u30D7\u30C8\u306B\u6E21\u3055\
  \u308C\u305F\u30D1\u30E9\u30E1\u30FC\u30BF\u3092\u51E6\u7406\u3059\u308B\u3053\u3068\
  \u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3053\u308C\u3092\u4F7F\
  \u3063\u3066\u67D4\u8EDF\u306B\u30B9\u30AF\u30EA\u30D7\u30C8\u3092\u5236\u5FA1\u3057\
  \u3001\u7570\u306A\u308B\u30B7\u30CA\u30EA\u30AA\u306B\u5BFE\u5FDC\u3057\u307E\u3059\
  \u3002"
title: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u306E\u8AAD\u307F\u53D6\
  \u308A"
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
