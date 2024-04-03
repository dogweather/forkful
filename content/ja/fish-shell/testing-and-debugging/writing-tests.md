---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:42.321070-07:00
description: "\u65B9\u6CD5\uFF1A Fish\u306B\u306F\u3001\u4ED6\u306E\u30D7\u30ED\u30B0\
  \u30E9\u30DF\u30F3\u30B0\u74B0\u5883\u306E\u3088\u3046\u306A\u7D44\u307F\u8FBC\u307F\
  \u306E\u30C6\u30B9\u30C8\u30D5\u30EC\u30FC\u30E0\u30EF\u30FC\u30AF\u306F\u3042\u308A\
  \u307E\u305B\u3093\u3002\u3057\u304B\u3057\u3001\u95A2\u6570\u306E\u52D5\u4F5C\u3092\
  \u30C1\u30A7\u30C3\u30AF\u3059\u308B\u30A2\u30B5\u30FC\u30B7\u30E7\u30F3\u3092\u4F7F\
  \u7528\u3059\u308B\u7C21\u5358\u306A\u30C6\u30B9\u30C8\u30B9\u30AF\u30EA\u30D7\u30C8\
  \u3092\u4F5C\u6210\u3067\u304D\u307E\u3059\u3002\u3055\u3089\u306B\u3001\u3088\u308A\
  \u5305\u62EC\u7684\u306A\u30C6\u30B9\u30C8\u30B9\u30A4\u30FC\u30C8\u306B\u306F\u3001\
  `fishtape`\u306E\u3088\u3046\u306A\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\u306E\
  \u30C4\u30FC\u30EB\u3092\u6D3B\u7528\u3067\u304D\u307E\u3059\u3002 #."
lastmod: '2024-03-13T22:44:42.740878-06:00'
model: gpt-4-0125-preview
summary: "Fish\u306B\u306F\u3001\u4ED6\u306E\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\
  \u74B0\u5883\u306E\u3088\u3046\u306A\u7D44\u307F\u8FBC\u307F\u306E\u30C6\u30B9\u30C8\
  \u30D5\u30EC\u30FC\u30E0\u30EF\u30FC\u30AF\u306F\u3042\u308A\u307E\u305B\u3093\u3002\
  \u3057\u304B\u3057\u3001\u95A2\u6570\u306E\u52D5\u4F5C\u3092\u30C1\u30A7\u30C3\u30AF\
  \u3059\u308B\u30A2\u30B5\u30FC\u30B7\u30E7\u30F3\u3092\u4F7F\u7528\u3059\u308B\u7C21\
  \u5358\u306A\u30C6\u30B9\u30C8\u30B9\u30AF\u30EA\u30D7\u30C8\u3092\u4F5C\u6210\u3067\
  \u304D\u307E\u3059\u3002\u3055\u3089\u306B\u3001\u3088\u308A\u5305\u62EC\u7684\u306A\
  \u30C6\u30B9\u30C8\u30B9\u30A4\u30FC\u30C8\u306B\u306F\u3001`fishtape`\u306E\u3088\
  \u3046\u306A\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\u306E\u30C4\u30FC\u30EB\u3092\
  \u6D3B\u7528\u3067\u304D\u307E\u3059."
title: "\u30C6\u30B9\u30C8\u306E\u4F5C\u6210"
weight: 36
---

## 方法：
Fishには、他のプログラミング環境のような組み込みのテストフレームワークはありません。しかし、関数の動作をチェックするアサーションを使用する簡単なテストスクリプトを作成できます。さらに、より包括的なテストスイートには、`fishtape`のようなサードパーティのツールを活用できます。

### 例1：基本的なテストスクリプト
2つの数値の合計を計算するFishの基本的な関数から始めましょう：

```fish
function add --description 'Add two numbers'
    set -l sum (math $argv[1] + $argv[2])
    echo $sum
end
```

この関数に対して基本的なテストスクリプトを次のように書くことができます：

```fish
function test_add
    set -l result (add 3 4)
    if test $result -eq 7
        echo "test_add passed"
    else
        echo "test_add failed"
    end
end

test_add
```

このスクリプトを実行すると、出力は以下のようになります：

```
test_add passed
```

### 例2：Fishtapeを使用して
より堅牢なテスト解決策には、Fish用のTAP生成テストランナーである`fishtape`を使用できます。

まず、まだインストールしていない場合は`fishtape`をインストールします：

```fish
fisher install jorgebucaran/fishtape
```

次に、`add`関数のためのテストファイルを作成します。例えば、`add_test.fish`：

```fish
test "Adding 3 and 4 yields 7"
    set result (add 3 4)
    echo "$result" | fishtape
end
```

テストを実行するには、以下のコマンドを使用します：

```fish
fishtape add_test.fish
```

サンプル出力は以下のようになるかもしれません：

```
TAP version 13
# Adding 3 and 4 yields 7
ok 1 - test_add passed
```

これはテストが成功したことを示しています。`fishtape`を使用すると、より詳細なテストを構築し、情報豊富な出力を提供してくれるため、デバッグが容易になり、Fishスクリプトの包括的なテストカバレッジを実現できます。
