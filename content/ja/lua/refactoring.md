---
title:                "リファクタリング"
date:                  2024-01-26T01:46:57.614399-07:00
model:                 gpt-4-0125-preview
simple_title:         "リファクタリング"
programming_language: "Lua"
category:             "Lua"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/refactoring.md"
---

{{< edit_this_page >}}

## 何となぜ？
リファクタリングとは、既存コードを少しずつ修正してその構造や可読性、効率を向上させる一方で、外部から見た振る舞いは変更しない技術のことです。プログラマーは、コードをより保守しやすくするため、複雑性を減らすため、または新しい機能を追加したりバグを修正する前に準備段階としてリファクタリングを行います。

## 方法：
簡単なLua関数を取り上げてリファクタリングしてみましょう。リスト内の数値の合計を計算する関数ですが、最初は効率や明確さをあまり考えずに書かれています：

```Lua
function sumList(numbers)
    local result = 0
    for i=1, #numbers do
        for j=1, #numbers do
            if i == j then
                result = result + numbers[i]
            end
        end
    end
    return result
end

print(sumList({1, 2, 3, 4})) -- 出力：10
```

より効率的で読みやすいバージョンにリファクタリングする：
```Lua
function sumListRefactored(numbers)
    local result = 0
    for _, value in ipairs(numbers) do
        result = result + value
    end
    return result
end

print(sumListRefactored({1, 2, 3, 4})) -- 依然として出力：10
```

リファクタリングされたバージョンは、冗長な内部ループを取り除き、`ipairs`を使ってリストをきれいに反復処理します。

## 深掘り
歴史的には、リファクタリングは80年代後半のSmalltalkプログラミングコミュニティから来ており、マーチン・ファウラーの書籍「Refactoring: Improving the Design of Existing Code」によって人気が出ました。Luaでは、リファクタリングは複雑な条件の単純化、大きな関数の小さなものへの分割、テーブルの使用の最適化を通じてパフォーマンスを向上させることがよく含まれます。

Luaでのリファクタリングには注意点があります。Luaの動的な性質と柔軟な型付けは、変数の名前の変更や関数シグネチャの変更など、注意深く行わないとリスクを伴うリファクタリングをすることになります。静的コード解析ツール（`luacheck`など）は、そのようなリスクを減らすことができます。代替手段には、テスト駆動開発（TDD）があります。ここではコードは開発プロセスの不可欠な部分として、別のリファクタリングフェーズとは対照的に、継続的にリファクタリングされます。

## 参照
- ベストプラクティスと例については、ロベルト・イェルサリムシーによる「Programming in Lua」。
- 言語横断的な原則については、マーチン・ファウラーによる「Refactoring: Improving the Design of Existing Code」。
- Luaコードの保守やリファクタリングを目的としたツールやモジュールについては、LuaRocksディレクトリ（https://luarocks.org/）。
