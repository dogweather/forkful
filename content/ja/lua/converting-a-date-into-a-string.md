---
title:                "日付を文字列に変換する"
html_title:           "C++: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？

日付を文字列に変換するとは、特定の日付フォーマット（例：YYYY-MM-DD）を保持する普通の文字列を生成することです。これにより、プログラマーは日付データを扱いやすくし、ユーザーフレンドリーな形式で表示できます。

## どうやって：

```Lua
os.date('%Y-%m-%d',os.time())   ---> 使用例
```
結果：
```Lua
2022-02-16   -- 例えば、現在の日付
```
os.date関数はフォーマット文字列と（オプションで）タイムスタンプを引数に取ります。指定しない場合、現在の日付と時間が使用されます。

## 深堀り：

過去、日付の文字列変換は手作業で行われ、エラーを生む元でした。しかしLuaはこれを簡単にし、os.date関数で高度に柔軟なフォーマットオプションを提供します。

代替案としては、特定のフォーマットに対するカスタム関数を作成することも可能ですが、それはエラーが起きやすく、再利用が困難であり、推奨されません。

os.dateの内部実装は、基本的なCライブラリ関数strftime()をラップすることで行われています。これは、クロスプラットフォームの標準であり、多くの言語で見られます。

## 関連情報：

Lua公式ドキュメンテーション：[os.date](https://www.lua.org/manual/5.3/manual.html#6.9) 

Stack Overflow：[Luaでの日付変換のヒント](https://stackoverflow.com/questions/9088267/date-format-in-lua) 

Lua-users wiki：[DatesAndTime](http://lua-users.org/wiki/DatesAndTime)