---
date: 2024-01-20 17:56:13.415352-07:00
description: "How to: (\u65B9\u6CD5) Fish Shell\u3067\u306F\u3001`$argv`\u306F\u30B7\
  \u30A7\u30EB\u30B9\u30AF\u30EA\u30D7\u30C8\u306B\u6E21\u3055\u308C\u305F\u5168\u3066\
  \u306E\u5F15\u6570\u3092\u542B\u3080\u30EA\u30B9\u30C8\u3067\u3059\u3002\u6B74\u53F2\
  \u7684\u306B\u3001\u4ED6\u306E\u30B7\u30A7\u30EB(Bash\u3084Zsh\u306A\u3069)\u3067\
  \u3082\u540C\u69D8\u306E\u6982\u5FF5\u304C\u3042\u308A\u307E\u3059\u304C\u3001Fish\u306F\
  \u30B7\u30F3\u30BF\u30C3\u30AF\u30B9\u304C\u72EC\u7279\u3067\u3059\u3002\u4F8B\u3048\
  \u3070\u3001Bash\u3067\u306F`$@`\u3084`$1`,\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.539580-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) Fish Shell\u3067\u306F\u3001`$argv`\u306F\u30B7\u30A7\u30EB\
  \u30B9\u30AF\u30EA\u30D7\u30C8\u306B\u6E21\u3055\u308C\u305F\u5168\u3066\u306E\u5F15\
  \u6570\u3092\u542B\u3080\u30EA\u30B9\u30C8\u3067\u3059\u3002\u6B74\u53F2\u7684\u306B\
  \u3001\u4ED6\u306E\u30B7\u30A7\u30EB(Bash\u3084Zsh\u306A\u3069)\u3067\u3082\u540C\
  \u69D8\u306E\u6982\u5FF5\u304C\u3042\u308A\u307E\u3059\u304C\u3001Fish\u306F\u30B7\
  \u30F3\u30BF\u30C3\u30AF\u30B9\u304C\u72EC\u7279\u3067\u3059\u3002\u4F8B\u3048\u3070\
  \u3001Bash\u3067\u306F`$@`\u3084`$1`, `$2`\u306E\u3088\u3046\u306B\u5F15\u6570\u3092\
  \u6271\u3044\u307E\u3059\u3002Fish\u3067\u306F\u3001\u7701\u7565\u5F62\u3084\u30A8\
  \u30E9\u30FC\u30CF\u30F3\u30C9\u30EA\u30F3\u30B0\u3082\u3088\u308A\u8AAD\u307F\u3084\
  \u3059\u304F\u66F8\u3051\u307E\u3059\u3002\u4F8B\u3048\u3070\u3001\u4F55\u3089\u304B\
  \u306E\u5F15\u6570\u304C\u5FC5\u8981\u306A\u5834\u5408\u3001\u6B21\u306E\u3088\u3046\
  \u306B\u66F8\u304F\u3053\u3068\u3082\u3067\u304D\u307E\u3059\u3002"
title: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u306E\u8AAD\u307F\u53D6\
  \u308A"
weight: 23
---

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
