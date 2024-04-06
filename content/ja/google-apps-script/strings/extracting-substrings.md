---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:53:03.739483-07:00
description: "\u65B9\u6CD5\uFF1A Google Apps Script\u3067\u306F\u3001\u73FE\u4EE3\u306E\
  JavaScript\u306B\u57FA\u3065\u3044\u3066\u3001`substring()`\u3001`substr()`\u3001\
  `slice()`\u3092\u542B\u3080\u3044\u304F\u3064\u304B\u306E\u65B9\u6CD5\u3092\u901A\
  \u3058\u3066\u3001\u6587\u5B57\u5217\u306E\u62BD\u51FA\u304C\u53EF\u80FD\u3067\u3059\
  \u3002\u305D\u308C\u305E\u308C\u306B\u30CB\u30E5\u30A2\u30F3\u30B9\u304C\u3042\u308A\
  \u307E\u3059\u304C\u3001\u6307\u5B9A\u3055\u308C\u305F\u6587\u5B57\u3092\u6587\u5B57\
  \u5217\u304B\u3089\u5F15\u304D\u51FA\u3059\u76EE\u7684\u3092\u3059\u3079\u3066\u679C\
  \u305F\u3057\u307E\u3059\u3002"
lastmod: '2024-04-05T22:37:49.760817-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A Google Apps Script\u3067\u306F\u3001\u73FE\u4EE3\u306E\
  JavaScript\u306B\u57FA\u3065\u3044\u3066\u3001`substring()`\u3001`substr()`\u3001\
  `slice()`\u3092\u542B\u3080\u3044\u304F\u3064\u304B\u306E\u65B9\u6CD5\u3092\u901A\
  \u3058\u3066\u3001\u6587\u5B57\u5217\u306E\u62BD\u51FA\u304C\u53EF\u80FD\u3067\u3059\
  \u3002\u305D\u308C\u305E\u308C\u306B\u30CB\u30E5\u30A2\u30F3\u30B9\u304C\u3042\u308A\
  \u307E\u3059\u304C\u3001\u6307\u5B9A\u3055\u308C\u305F\u6587\u5B57\u3092\u6587\u5B57\
  \u5217\u304B\u3089\u5F15\u304D\u51FA\u3059\u76EE\u7684\u3092\u3059\u3079\u3066\u679C\
  \u305F\u3057\u307E\u3059\u3002"
title: "\u90E8\u5206\u6587\u5B57\u5217\u306E\u62BD\u51FA"
weight: 6
---

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
