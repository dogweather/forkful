---
title:                "Ruby: 文字列の大文字化"
simple_title:         "文字列の大文字化"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## なぜ
文字列を大文字にすることに関わる理由は、メッセージや文書を強調したい場合や、データの整理や分類をする際に役立ちます。

## 方法
文字列を大文字にするには、Rubyのcapitalizeメソッドを使用します。下記のコード例をご確認ください。

```Ruby
# 変数に文字列を代入する
str = "hello world"

# capitalizeメソッドを使用して大文字に変換する
str.capitalize

# 出力結果: "Hello world"
```

## 深堀り
文字列を大文字にする際、空白や句読点などの特殊文字も大文字に変換されることに注意してください。また、日本語の文字列を大文字に変換する場合は、Stringクラスのcapitalizeメソッドではなく、UnicodeUtilsライブラリの方法を使用する必要があります。詳細は下記のリンクをご参照ください。

## 参考リンク
- [RubyのStringクラスAPI](https://docs.ruby-lang.org/ja/latest/class/String.html#I_CAPITALIZE)
- [UnicodeUtilsライブラリ](https://github.com/lang/unicode_utils)
- [文字列操作のチートシート](https://qiita.com/jnchito/items/268ae0059c98a3fce68b)

## 参考文献
- ["Rubyではじめるプログラミング" by 松前洋一郎・嶋田淳](https://www.amazon.co.jp/dp/4797383925/ref=cm_sw_r_tw_dp_U_x_rV3lBbSCR6CJC)