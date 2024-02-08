---
title:                "部分文字列の抽出"
aliases:
- ja/google-apps-script/extracting-substrings.md
date:                  2024-02-01T21:53:03.739483-07:00
model:                 gpt-4-0125-preview
simple_title:         "部分文字列の抽出"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/google-apps-script/extracting-substrings.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？

文字列の抽出は、文字列の一部を取り出すことを意味します。本質的には、既存の文字列の一部分から新しい文字列を作成しています。プログラマーは、データの解析、ユーザーインターフェースのテキスト操作、さまざまなアプリケーションへの入力処理など、多くの理由でこれを行います。これにより、文字列の抽出は、あらゆるスクリプトアーセナルの中で汎用的なツールとなっています。

## 方法：

Google Apps Scriptでは、現代のJavaScriptに基づいて、`substring()`、`substr()`、`slice()`を含むいくつかの方法を通じて、文字列の抽出が可能です。それぞれにニュアンスがありますが、指定された文字を文字列から引き出す目的をすべて果たします。

```javascript
// substring()を使用した例
var str = "Hello, world!";
var result = str.substring(0, 5);
console.log(result); // 出力：Hello

// substr()を使用した例
var resultSubstr = str.substr(7, 5);
console.log(resultSubstr); // 出力：world

// slice()を使用した例
var resultSlice = str.slice(-6);
console.log(resultSlice); // 出力：world!
```

各メソッドは2つの引数を取ります：開始位置と、`slice()`の場合は負のインデックスを受け入れることができますが、終了位置または抽出する文字の数です。これらの操作後に元の文字列が変更されないことに注意する価値があります。新しい文字列の値を返します。

## 詳細解説

歴史的に、文字列を抽出するためのJavaScriptメソッドは、類似した名前と機能により混乱の原因となっていました。しかし、Google Apps Scriptや現代のJavaScriptでは、`substring()`と`slice()`が最も頻繁に使用され、`substr()`は非推奨と見なされています。これは、将来的にも安全なコードを書くために重要です。

`substring()`と`slice()`の主な違いは、負のインデックスの扱い方にあります。`substring()`は負のインデックスを0として扱いますが、`slice()`は負のインデックスを受け入れ、文字列の末尾から抽出を開始することができます。これは、文字列の正確な長さがわからない場合や、末尾から抽出する必要がある場合に特に便利です。

文字列の抽出にどのメソッドを使用するかは、操作の具体的な要件（例えば、負のインデックスを扱うことが有益かどうかなど）や個人またはチームのコーディング標準によって決まることがよくあります。ベストプラクティスに一つの解答はありませんが、微妙な違いやパフォーマンスへの影響を理解することで、情報に基づいた決定を下すことができます。
