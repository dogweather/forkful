---
title:                "テストの作成"
html_title:           "Bash: テストの作成"
simple_title:         "テストの作成"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
テストコーディングとは、プログラムが意図した通りに動作するかを検証するプロセスです。プログラマーはバグを早期発見、未来のエラーを未然に防ぐためにテストを書きます。

## How to:
Fish Shellでのテストは通常、`functions`と`test`の組み合わせで行われます。下記は簡単な例です。

```Fish Shell
function add
    echo $argv[1] + $argv[2] | bc
end

test add
    if test (add 3 5) -eq 8
        echo "add function works"
    end
end

add 3 5  # 出力: 8
```

サンプル出力:
```
add function works
```

## Deep Dive
Fish Shellのテストサポートはまだ新しく、他のシェルスクリプト言語のフレームワークに比べるとシンプルです。BashではBatsがあり、ZshではZUnitが利用されることがあります。Fish Shellでは、組み込みの`test`コマンドで基本的なアサーションが利用できますが、複雑なテストが必要な場合は外部ツールを使うこともあります。

## See Also
- [Fish Shellの公式ドキュメント](https://fishshell.com/docs/current/index.html)
- [Bats: Bash Automated Testing System](https://github.com/bats-core/bats-core)
- [ZUnit: A powerful unit testing framework for Zsh](https://github.com/zunit-zsh/zunit)
