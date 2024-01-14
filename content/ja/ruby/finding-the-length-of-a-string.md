---
title:    "Ruby: 文字列の長さを見つける"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## なぜ
文字列の長さを求めることに参加したいのか、その理由をご紹介します。 

## 方法
文字列の長さを求めるには、 `string.length` というメソッドを使うことができます。これは、文字列の中の文字の数を返します。例えば、`"Hello".length` は 5 を返します。 

```Ruby
string = "こんにちは"
puts string.length
# output: 5
```

## 深堀り 
文字列の長さを求める際に、注意すべき点があります。一見、単純な作業に見えますが、Rubyでは文字列の中に日本語が含まれている場合、文字数をカウントする方法が異なります。具体的には、`"こんにちは".length` というコードは、正しくは4を返します。これは、日本語1文字がUTF-8で3バイトであるためです。そのため、文字列の長さを正確に求める場合は、`string.mb_chars.length` というメソッドを使う必要があります。 

```Ruby
string = "こんにちは"
puts string.mb_chars.length
# output: 4
```

## 参考リンク 
ここからは、文字列の長さを求める際に役立つ記事や公式ドキュメントをご紹介します。 

### 色々な言語での文字数カウント方法が紹介されています 
https://www.techiedelight.com/find-length-string-given-php-java-python-ruby/ 

### 日本語を含む文字列の長さを求める方法が詳しく説明されています 
https://taisukef.medium.com/ruby%E3%81%A7%E3%83%81%E3%83%A3%E3%83%BC%E3%82%AB%E3%82%A6%E3%83%B3%E3%83%88%E3%81%AE%E9%95%B7%E3%81%95%E3%82%92%E6%B1%82%E3%82%81%E3%82%8B-%E6%97%A5%E6%9C%AC%E8%AA%9E%E5%90%AB%E3%82%93%E3%81%AA%E6%96%87%E5%AD%97%E3%81%AE%E9%95%B7%E3%81%95%E3%82%92%E6%B1%82%E3%82%81%E3%82%8B-3b2e10f8c8bf 

### 公式ドキュメント 
https://ruby-doc.org/core-2.7.2/String.html#method-i-length 

## 参照 
https://www.rubyguides.com/2015/05/finding-the-length-of-a-string/