---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:42.321070-07:00
description: "Fish Shell\u3067\u306E\u30C6\u30B9\u30C8\u4F5C\u6210\u306F\u3001\u30B3\
  \u30FC\u30C9\u3092\u81EA\u52D5\u7684\u306B\u5B9F\u884C\u3057\u3066\u3001\u671F\u5F85\
  \u3055\u308C\u308B\u7D50\u679C\u3068\u6BD4\u8F03\u3057\u3066\u52D5\u4F5C\u3092\u691C\
  \u8A3C\u3059\u308B\u30B9\u30AF\u30EA\u30D7\u30C8\u3092\u4F5C\u6210\u3059\u308B\u3053\
  \u3068\u3092\u542B\u307F\u307E\u3059\u3002\u3053\u306E\u5B9F\u8DF5\u306F\u91CD\u8981\
  \u3067\u3042\u308A\u3001\u30B7\u30A7\u30EB\u30B9\u30AF\u30EA\u30D7\u30C8\u304C\u610F\
  \u56F3\u3057\u305F\u3068\u304A\u308A\u306B\u52D5\u4F5C\u3059\u308B\u3053\u3068\u3092\
  \u4FDD\u8A3C\u3057\u3001\u65E9\u671F\u306B\u30A8\u30E9\u30FC\u3092\u6355\u6349\u3057\
  \u3001\u30E1\u30F3\u30C6\u30CA\u30F3\u30B9\u3092\u5BB9\u6613\u306B\u3057\u307E\u3059\
  \u3002"
lastmod: '2024-02-25T18:49:40.684246-07:00'
model: gpt-4-0125-preview
summary: "Fish Shell\u3067\u306E\u30C6\u30B9\u30C8\u4F5C\u6210\u306F\u3001\u30B3\u30FC\
  \u30C9\u3092\u81EA\u52D5\u7684\u306B\u5B9F\u884C\u3057\u3066\u3001\u671F\u5F85\u3055\
  \u308C\u308B\u7D50\u679C\u3068\u6BD4\u8F03\u3057\u3066\u52D5\u4F5C\u3092\u691C\u8A3C\
  \u3059\u308B\u30B9\u30AF\u30EA\u30D7\u30C8\u3092\u4F5C\u6210\u3059\u308B\u3053\u3068\
  \u3092\u542B\u307F\u307E\u3059\u3002\u3053\u306E\u5B9F\u8DF5\u306F\u91CD\u8981\u3067\
  \u3042\u308A\u3001\u30B7\u30A7\u30EB\u30B9\u30AF\u30EA\u30D7\u30C8\u304C\u610F\u56F3\
  \u3057\u305F\u3068\u304A\u308A\u306B\u52D5\u4F5C\u3059\u308B\u3053\u3068\u3092\u4FDD\
  \u8A3C\u3057\u3001\u65E9\u671F\u306B\u30A8\u30E9\u30FC\u3092\u6355\u6349\u3057\u3001\
  \u30E1\u30F3\u30C6\u30CA\u30F3\u30B9\u3092\u5BB9\u6613\u306B\u3057\u307E\u3059\u3002"
title: "\u30C6\u30B9\u30C8\u306E\u4F5C\u6210"
---

{{< edit_this_page >}}

## 何となぜ？

Fish Shellでのテスト作成は、コードを自動的に実行して、期待される結果と比較して動作を検証するスクリプトを作成することを含みます。この実践は重要であり、シェルスクリプトが意図したとおりに動作することを保証し、早期にエラーを捕捉し、メンテナンスを容易にします。

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
