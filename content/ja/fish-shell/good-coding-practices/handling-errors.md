---
date: 2024-01-26 00:52:52.304542-07:00
description: "\u3069\u306E\u3088\u3046\u306B\uFF1A Fish\u3067\u30A8\u30E9\u30FC\u3092\
  \u30AD\u30E3\u30C3\u30C1\u3059\u308B\u306B\u306F\u3001`status`\u30B3\u30DE\u30F3\
  \u30C9\u3068\u6761\u4EF6\u5F0F\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002\u4F8B\u3048\
  \u3070`ping`\u304C\u5931\u6557\u3057\u305F\u5834\u5408\u3001\u305D\u306E\u691C\u51FA\
  \u65B9\u6CD5\u306F\u4EE5\u4E0B\u306E\u901A\u308A\u3067\u3059\uFF1A."
lastmod: '2024-03-13T22:44:42.747521-06:00'
model: gpt-4-1106-preview
summary: "Fish\u3067\u30A8\u30E9\u30FC\u3092\u30AD\u30E3\u30C3\u30C1\u3059\u308B\u306B\
  \u306F\u3001`status`\u30B3\u30DE\u30F3\u30C9\u3068\u6761\u4EF6\u5F0F\u3092\u4F7F\
  \u7528\u3057\u307E\u3059\u3002\u4F8B\u3048\u3070`ping`\u304C\u5931\u6557\u3057\u305F\
  \u5834\u5408\u3001\u305D\u306E\u691C\u51FA\u65B9\u6CD5\u306F\u4EE5\u4E0B\u306E\u901A\
  \u308A\u3067\u3059\uFF1A."
title: "\u30A8\u30E9\u30FC\u51E6\u7406"
weight: 16
---

## どのように：
Fishでエラーをキャッチするには、`status`コマンドと条件式を使用します。例えば`ping`が失敗した場合、その検出方法は以下の通りです：

```fish
ping -c 1 example.com
if not status is-success
    echo "Something fishy happened with the ping."
end
```

`ping`が失敗した場合のサンプル出力：

```
Something fishy happened with the ping.
```

特定のエラーコードを処理するには、`status --is`を使用します：

```fish
false
if status --is 1
    echo "Caught an error with code 1."
end
```

サンプル出力：
```
Caught an error with code 1.
```

より堅牢なアプローチを考える場合は、関数の使用を検討してください：

```fish
function try_ping
    ping -c 1 example.com
    or begin
        echo "Ping failed with status $status"
        return 1
    end
end

try_ping
```

## 詳細な検討
Fishでのエラー処理は、高級言語で知られる`try/catch`パラダイムには合いません。代わりに、`status`コマンドによって提供されるシンプルな終了ステータスを使用します。

歴史的に、Unix系システムでは、終了ステータスが`0`であれば成功を意味し、非ゼロ値はエラーを示します。これは様々な失敗の理由を反映していることが多く、ほとんどのコマンドラインユーティリティーによって使用されているため、Fish自体にも採用されています。

Fishでの`status`チェックに代わる方法としては、他のシェルでのシグナル処理による`trap`がありますが、Fishは副作用が少なく、より明確なステータスチェックを好みます。

実装に関しては、Fishでのエラー処理は、ブロックしない性質と明確な構文への重点に大きく貢献され、例で示されたようにシンプルでありながら強力です。エラーコードは関数とうまく組み合わせることができ、モジュール化され読みやすいエラー管理を可能にします。

## 参考情報
- Fishの条件文に関するドキュメント：https://fishshell.com/docs/current/language.html#conditionals
- Fishにおけるエラー処理のチュートリアル：https://fishshell.com/docs/current/tutorial.html#error-handling
