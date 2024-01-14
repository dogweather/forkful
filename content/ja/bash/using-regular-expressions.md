---
title:                "Bash: 正規表現を使用する"
simple_title:         "正規表現を使用する"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/using-regular-expressions.md"
---

{{< edit_this_page >}}

## なぜ
正規表現を使用する理由は多々あります。まず、テキストのパターンマッチングや検索、置換などの処理を容易にすることができます。また、テキスト処理を行う際には、正規表現を使用することでプログラムの柔軟性が向上し、コード量も削減できます。

## 使い方
正規表現の基本的な使い方を紹介します。まずは```Bash```コマンドを使用して、正規表現を利用したテキスト処理の例を見てみましょう。

例1: 文字列のマッチング
```Bash
text="こんにちは、世界"
if [[ $text =~ ^こん.*界$ ]]; then
    echo "マッチしました！"
else
    echo "マッチしませんでした。"
fi
```

出力:
```Bash
マッチしました！
```

この例では、```=~```演算子を使用して、正規表現パターン```^こん.*界$```と文字列```こんにちは、世界```をマッチングしています。正規表現パターンの先頭の```^```は、マッチングする文字列の先頭を表し、```$```は末尾を表します。また、```.```は任意の文字を表し、```*```は直前の文字が0回以上繰り返されることを表します。つまり、この例では、「こん」の後ろに任意の文字が0回以上繰り返され、最後に「界」が続く文字列とマッチングします。

例2: 文字列の置換
```Bash
text="こんにちは、世界"
echo ${text/こんにちは/Hello}
```

出力:
```Bash
Hello、世界
```

この例では、```/${oldString}/${newString}```の形式で、文字列の置換を行っています。正規表現パターンを使用することで、様々な文字列の置換が可能となります。

## 深堀り
正規表現を使用する際に注意するべき点があります。ひとつはメタ文字と呼ばれる特殊な文字の扱いです。例えば、```\```や```/```などはメタ文字として扱われるため、文字列として取り扱いたい場合にはエスケープする必要があります。また、正規表現は「最良のマッチ」を返すため、意図しないマッチングが行われないように、正規表現パターンを慎重に作成する必要があります。

## 参考リンク
- [正規表現入門 (機械学習ゼミ)](http://deeplearning.jp/seminar-42/)
- [Bashでの正規表現の使い方 (Qiita)](https://qiita.com/goroppo/items/1b3d60833d08b524f64a)
- [正規表現の基本 (Wikipedia)](https://ja.wikipedia.org/wiki/%E6%AD%A3%E8%A6%8F%E8%A1%A8%E7%8F%BE)

## 参考になるリンク
- [Markdownの使い方（日本語） (Qiita)](https://qiita.com/higuma/items/3f7965cf1973d9215909)
- [Bashの基本文法 (dev.classmethod.jp)](https://dev.classmethod.jp/server-side/bash-basic/)