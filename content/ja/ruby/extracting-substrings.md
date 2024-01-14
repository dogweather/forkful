---
title:    "Ruby: 「部分文字列の抽出」"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

# なぜ: Substringsの抽出に取り組むのか

Substringsの抽出は、文字列の一部を抽出するための重要なテクニックです。例えば、ある文字列から特定の文字列を見つけたり、特定の位置から文字列を切り取ったりすることができます。これにより、より複雑な操作を必要とする文字列処理をより簡単に行うことができます。

## 方法: Substringsの抽出方法

Substringsの抽出には、Stringクラスの`slice`や`[]`メソッドを使用します。例えば、以下のように書くことで、ある文字列から特定の部分文字列を抽出することができます。

```Ruby
str = "こんにちは、世界！"
puts str.slice(3, 5)   # ちは、世界！
puts str[0, 5]         # こん
```

上記の例では、`slice`メソッドと`[]`演算子の両方が使用できますが、どちらも同じ結果になります。`slice`メソッドの第一引数には開始位置、第二引数には文字数を指定します。`[]`演算子の場合には、開始位置と文字数を指定する代わりに、範囲指定を行うことができます。

## 深堀り: Substringsの抽出について

Substringsの抽出には、さまざまな方法やオプションがあります。たとえば、`slice`メソッドや`[]`演算子の他にも、正規表現やメソッドチェーンを使用することもできます。また、`slice`メソッドや`[]`演算子には、文字列の最後から数えた位置を指定することもできます。

さらに、substringを抽出する際には、文字列の文字コードに気を付ける必要があります。日本語の文字の場合、半角の英数字と比べると文字コードが異なるため、うまく抽出できない場合があります。

# 参考リンク

- [Rubyによるsubstringの抽出方法 (Qiita)](https://qiita.com/shizuma/items/4fd7176396b433e0bd1b)
- [Rubyの文字列処理 (Ruby On Rails ショートカット)](https://railsguides.jp/active_support_core_extensions.html#%E6%96%87%E5%AD%97%E5%88%97%E3%81%A8%E3%81%AE%E9%96%A2%E4%BF%82%E3%81%AE%E6%8B%A1%E5%BC%B5)
- [Rubyの正規表現の基礎 (TechAcademy Magazine)](https://techacademy.jp/magazine/28111)