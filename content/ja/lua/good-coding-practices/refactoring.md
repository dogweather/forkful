---
aliases:
- /ja/lua/refactoring/
date: 2024-01-26 01:46:57.614399-07:00
description: "\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0\u3068\u306F\u3001\u65E2\
  \u5B58\u30B3\u30FC\u30C9\u3092\u5C11\u3057\u305A\u3064\u4FEE\u6B63\u3057\u3066\u305D\
  \u306E\u69CB\u9020\u3084\u53EF\u8AAD\u6027\u3001\u52B9\u7387\u3092\u5411\u4E0A\u3055\
  \u305B\u308B\u4E00\u65B9\u3067\u3001\u5916\u90E8\u304B\u3089\u898B\u305F\u632F\u308B\
  \u821E\u3044\u306F\u5909\u66F4\u3057\u306A\u3044\u6280\u8853\u306E\u3053\u3068\u3067\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30B3\u30FC\u30C9\u3092\
  \u3088\u308A\u4FDD\u5B88\u3057\u3084\u3059\u304F\u3059\u308B\u305F\u3081\u3001\u8907\
  \u96D1\u6027\u3092\u6E1B\u3089\u3059\u305F\u3081\u3001\u307E\u305F\u306F\u65B0\u3057\
  \u3044\u6A5F\u80FD\u3092\u8FFD\u52A0\u3057\u305F\u308A\u30D0\u30B0\u3092\u4FEE\u6B63\
  \u3059\u308B\u524D\u306B\u6E96\u5099\u6BB5\u968E\u3068\u3057\u3066\u30EA\u30D5\u30A1\
  \u30AF\u30BF\u30EA\u30F3\u30B0\u3092\u884C\u3044\u307E\u3059\u3002"
lastmod: 2024-02-18 23:08:55.041605
model: gpt-4-0125-preview
summary: "\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0\u3068\u306F\u3001\u65E2\
  \u5B58\u30B3\u30FC\u30C9\u3092\u5C11\u3057\u305A\u3064\u4FEE\u6B63\u3057\u3066\u305D\
  \u306E\u69CB\u9020\u3084\u53EF\u8AAD\u6027\u3001\u52B9\u7387\u3092\u5411\u4E0A\u3055\
  \u305B\u308B\u4E00\u65B9\u3067\u3001\u5916\u90E8\u304B\u3089\u898B\u305F\u632F\u308B\
  \u821E\u3044\u306F\u5909\u66F4\u3057\u306A\u3044\u6280\u8853\u306E\u3053\u3068\u3067\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30B3\u30FC\u30C9\u3092\
  \u3088\u308A\u4FDD\u5B88\u3057\u3084\u3059\u304F\u3059\u308B\u305F\u3081\u3001\u8907\
  \u96D1\u6027\u3092\u6E1B\u3089\u3059\u305F\u3081\u3001\u307E\u305F\u306F\u65B0\u3057\
  \u3044\u6A5F\u80FD\u3092\u8FFD\u52A0\u3057\u305F\u308A\u30D0\u30B0\u3092\u4FEE\u6B63\
  \u3059\u308B\u524D\u306B\u6E96\u5099\u6BB5\u968E\u3068\u3057\u3066\u30EA\u30D5\u30A1\
  \u30AF\u30BF\u30EA\u30F3\u30B0\u3092\u884C\u3044\u307E\u3059\u3002"
title: "\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0"
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
