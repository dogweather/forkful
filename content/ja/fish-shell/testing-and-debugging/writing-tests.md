---
title:                "テストの作成"
date:                  2024-02-03T19:30:42.321070-07:00
model:                 gpt-4-0125-preview
simple_title:         "テストの作成"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
