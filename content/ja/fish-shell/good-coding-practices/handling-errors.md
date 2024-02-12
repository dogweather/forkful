---
title:                "エラー処理"
date:                  2024-01-26T00:52:52.304542-07:00
model:                 gpt-4-1106-preview
simple_title:         "エラー処理"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/handling-errors.md"
---

{{< edit_this_page >}}

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
