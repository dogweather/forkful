---
title:                "テキストファイルの読み込み"
html_title:           "Lua: テキストファイルの読み込み"
simple_title:         "テキストファイルの読み込み"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/reading-a-text-file.md"
---

{{< edit_this_page >}}

## なに? なんで?
テキストファイルを読み込むとは、プログラマーがコンピューター上で文字列データを取得することです。これを行う主な理由は、プログラム内で外部ファイルからデータを利用したり、テキストファイルを編集することです。

## 使い方:
```lua
-- ファイルを開く
local file = io.open("data.txt", "r")

-- ファイル内のテキストを読み込む
local text = file:read("*all")

-- ファイルを閉じる
file:close()

print(text) -- "こんにちは、世界！"
```
上記のコードでは、"data.txt"という名前のテキストファイルを読み込み、その内容をコンソール上に出力しています。

## 詳しく調べる:
テキストファイルの読み込みは、プログラミングの世界では一般的なタスクです。歴史的には、プログラム内でテキストファイルを扱うために開発された特殊なツールが使用されていましたが、現在では言語やライブラリの機能として組み込まれています。また、テキストファイルの読み込みにはいくつかの方法がありますが、Luaではioライブラリを使用するのが一般的です。このライブラリを使用することで、ファイルを開く、読み込む、閉じるといった一連の操作が可能になります。

## 関連リンク:
- [Lua公式ドキュメント](https://www.lua.org/pil/21.html)
- [テキストファイルの読み込み方法の比較](https://www.linuxjournal.com/content/reading-and-writing-files-lua)