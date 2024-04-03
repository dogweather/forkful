---
date: 2024-01-26 00:52:52.304542-07:00
description: "\u30A8\u30E9\u30FC\u51E6\u7406\u306F\u3001\u30B9\u30AF\u30EA\u30D7\u30C8\
  \u304C\u4E88\u671F\u305B\u306C\u554F\u984C\u306B\u5BFE\u3057\u3066\u3001\u3046\u307E\
  \u304F\u5BFE\u5FDC\u3067\u304D\u308B\u3088\u3046\u306B\u3059\u308B\u3082\u306E\u3067\
  \u3059\u3002\u5931\u6557\u3092\u7BA1\u7406\u3057\u3001\u30E6\u30FC\u30B6\u30FC\u306E\
  \u9AEA\u3092\u7070\u8272\u306B\u3055\u305B\u308B\u3053\u3068\u306A\u304F\u5BFE\u5FDC\
  \u3059\u308B\u305F\u3081\u306B\u884C\u3044\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.747521-06:00'
model: gpt-4-1106-preview
summary: "\u30A8\u30E9\u30FC\u51E6\u7406\u306F\u3001\u30B9\u30AF\u30EA\u30D7\u30C8\
  \u304C\u4E88\u671F\u305B\u306C\u554F\u984C\u306B\u5BFE\u3057\u3066\u3001\u3046\u307E\
  \u304F\u5BFE\u5FDC\u3067\u304D\u308B\u3088\u3046\u306B\u3059\u308B\u3082\u306E\u3067\
  \u3059\u3002\u5931\u6557\u3092\u7BA1\u7406\u3057\u3001\u30E6\u30FC\u30B6\u30FC\u306E\
  \u9AEA\u3092\u7070\u8272\u306B\u3055\u305B\u308B\u3053\u3068\u306A\u304F\u5BFE\u5FDC\
  \u3059\u308B\u305F\u3081\u306B\u884C\u3044\u307E\u3059\u3002."
title: "\u30A8\u30E9\u30FC\u51E6\u7406"
weight: 16
---

## 何となぜ？
エラー処理は、スクリプトが予期せぬ問題に対して、うまく対応できるようにするものです。失敗を管理し、ユーザーの髪を灰色にさせることなく対応するために行います。

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
