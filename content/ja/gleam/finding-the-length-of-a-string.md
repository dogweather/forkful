---
title:                "文字列の長さを見つける"
html_title:           "Gleam: 文字列の長さを見つける"
simple_title:         "文字列の長さを見つける"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## なぜ
文字列の長さを調べることの利点を知りたい人にとって、Gleamの機能を活用することができるように実用的な方法を説明します。

## 方法
```Gleam
//文字列の長さを調べる関数
fn string_length(string) {
    //文の終わりのnull文字を除外する
    let adjusted_string = string.trim_end(null);
    //調べたい文字列の長さを返す
    std.string.length(adjusted_string)
}

//例：文字列の長さを調べる
let input_string = "こんにちは！";
stdio.println(string_length(input_string));

//出力：5
```

## 深堀り
文字列の長さを調べることは、文字列を操作する上で非常に重要です。Gleamの`std.string`モジュールには、標準的な文字列操作機能が組み込まれており、特に`length`関数は文字列を扱う上で便利な機能です。また、文字列の長さを調べる際には、文の終わりのnull文字を除外することも重要です。

## 参考リンク
- Gleam 公式ドキュメント: https://gleam.run/documentation/
- 文字列操作のための`std.string`モジュールの詳細: https://gleam.run/documentation/stdlib/stdlib-string/