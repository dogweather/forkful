---
title:                "ランダムな数値を生成する"
html_title:           "Lua: ランダムな数値を生成する"
simple_title:         "ランダムな数値を生成する"
programming_language: "Lua"
category:             "Lua"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/generating-random-numbers.md"
---

{{< edit_this_page >}}

## コーディングにおけるランダム数値の生成とその必要性

ランダム数値の生成とは、プログラマーがコンピューターに対してランダムな数値を生成することを指します。プログラマーはこの技術を利用して、ランダム性を持ったプログラムを作成し、データや処理の偏りを避けることができます。

## やり方：

```Lua
math.randomseed(os.time())
print(math.random(1, 10)) -- 1から10の間でランダムな数値を生成して出力
```

```Lua
for i = 1, 5 do
  print(math.random(1, 10)) -- ループを使用して1から10の間で5回ランダムな数値を生成して出力
end
```

## 詳細説明：

ランダム数値がプログラミングにおいて重要なのは、データや処理の偏りを避けるためです。例えば、サイコロを1回振るときに必ず6が出るようにプログラミングするのは間違っています。そのため、ランダム数値を使用して、プログラムの結果を不確定性のあるものにし、バグやエラーを回避することができます。

また、古いバージョンのLuaでは、math.random関数は線形合同法というアルゴリズムを使用していました。しかし、現在のバージョンではメルセンヌ・ツイスターというアルゴリズムを使用しています。このアルゴリズムはよりランダム性が高く、より多くの数値を生成することができます。

## 関連情報：

- [Lua 公式ドキュメント](https://www.lua.org/docs.html)：Luaプログラミング言語の公式ドキュメント。
- [math.randomの仕様](https://www.lua.org/manual/5.3/manual.html#pdf-math.random)：Lua公式ドキュメントにおけるmath.random関数の仕様を確認することができます。
- [乱数生成アルゴリズム](https://ja.wikipedia.org/wiki/乱数生成アルゴリズム)：線形合同法やメルセンヌ・ツイスターなど、さまざまな乱数生成アルゴリズムの説明を読むことができます。