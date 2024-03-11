---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:11:58.466945-07:00
description: "JavaScript\u3067\u306F\u3001\u9023\u60F3\u914D\u5217\u3068\u3057\u3066\
  \u77E5\u3089\u308C\u308B\u3082\u306E\u306F\u3001\u3088\u308A\u6B63\u78BA\u306B\u306F\
  \u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\u3068\u547C\u3070\u308C\u3066\u304A\u308A\u3001\
  \u30AD\u30FC\u306B\u5024\u3092\u30DE\u30C3\u30D4\u30F3\u30B0\u3059\u308B\u3053\u3068\
  \u304C\u3067\u304D\u307E\u3059\u3002\u3053\u308C\u306F\u3001\u8981\u7D20\u306E\u96C6\
  \u307E\u308A\u3092\u7279\u5B9A\u306E\u540D\u524D\uFF08\u30AD\u30FC\uFF09\u3092\u4ECB\
  \u3057\u3066\u30A2\u30AF\u30BB\u30B9\u3057\u305F\u3044\u5834\u5408\u306B\u975E\u5E38\
  \u306B\u4FBF\u5229\u3067\u3042\u308A\u3001\u30B3\u30FC\u30C9\u3092\u3088\u308A\u8AAD\
  \u307F\u3084\u3059\u304F\u67D4\u8EDF\u306B\u3057\u307E\u3059\u3002"
lastmod: '2024-03-11T00:14:16.211514-06:00'
model: gpt-4-0125-preview
summary: "JavaScript\u3067\u306F\u3001\u9023\u60F3\u914D\u5217\u3068\u3057\u3066\u77E5\
  \u3089\u308C\u308B\u3082\u306E\u306F\u3001\u3088\u308A\u6B63\u78BA\u306B\u306F\u30AA\
  \u30D6\u30B8\u30A7\u30AF\u30C8\u3068\u547C\u3070\u308C\u3066\u304A\u308A\u3001\u30AD\
  \u30FC\u306B\u5024\u3092\u30DE\u30C3\u30D4\u30F3\u30B0\u3059\u308B\u3053\u3068\u304C\
  \u3067\u304D\u307E\u3059\u3002\u3053\u308C\u306F\u3001\u8981\u7D20\u306E\u96C6\u307E\
  \u308A\u3092\u7279\u5B9A\u306E\u540D\u524D\uFF08\u30AD\u30FC\uFF09\u3092\u4ECB\u3057\
  \u3066\u30A2\u30AF\u30BB\u30B9\u3057\u305F\u3044\u5834\u5408\u306B\u975E\u5E38\u306B\
  \u4FBF\u5229\u3067\u3042\u308A\u3001\u30B3\u30FC\u30C9\u3092\u3088\u308A\u8AAD\u307F\
  \u3084\u3059\u304F\u67D4\u8EDF\u306B\u3057\u307E\u3059\u3002"
title: "\u9023\u60F3\u914D\u5217\u306E\u4F7F\u7528"
---

{{< edit_this_page >}}

## 何となぜ？

JavaScriptでは、連想配列として知られるものは、より正確にはオブジェクトと呼ばれており、キーに値をマッピングすることができます。これは、要素の集まりを特定の名前（キー）を介してアクセスしたい場合に非常に便利であり、コードをより読みやすく柔軟にします。

## 方法：

JavaScriptで連想配列（オブジェクト）を作成および使用することは簡単です。波括弧`{}`でオブジェクトを定義し、その中でキーと値の組を定義できます。キーは常に文字列であり、値には何でも使用できます：文字列、数値、配列、他のオブジェクトさえも。

```javascript
// 連想配列の作成
let userInfo = {
  name: "Alex",
  age: 30,
  email: "alex@example.com"
};

// 要素へのアクセス
console.log(userInfo.name); // 出力: Alex
console.log(userInfo["email"]); // 出力: alex@example.com

// 新しい要素の追加
userInfo.job = "Developer";
userInfo["country"] = "Canada";

console.log(userInfo);
/* 出力:
{
  name: "Alex",
  age: 30,
  email: "alex@example.com",
  job: "Developer",
  country: "Canada"
}
*/

// 要素の削除
delete userInfo.age;
console.log(userInfo);
/* 出力:
{
  name: "Alex",
  email: "alex@example.com",
  job: "Developer",
  country: "Canada"
}
*/
```

ご覧の通り、連想配列の要素にアクセスしたり、追加したり、削除したりすることは非常に直接的で直感的です。

## 深堀り

JavaScriptの世界では、「連想配列」という用語がよく聞かれますが、JavaScriptには他の言語（例：PHP）のような真の連想配列は存在しないため、厳密には誤用です。JavaScriptには、似た目的を果たすが、より強力で柔軟な構造を持つオブジェクトがあります。

歴史的に、プログラミング言語の配列は、アイテムの集まりを数値インデックスでアクセスできるように設計されていました。しかし、ソフトウェア開発が進化するにつれて、より柔軟なデータ構造が必要になりました。連想配列や他の言語の辞書などは、任意のキーを通じて要素にアクセスできるという応答の一つです。

キーと値のストアとしてのオブジェクトによるJavaScriptのアプローチは、機能性のブレンドを提供します。プロパティ（キー）の追加、削除、名前での検索が可能です。JSON(JavaScript Object Notation)は、この構造の有用性を証明しており、ウェブ上でのデータ交換の事実上の標準になっています。

オブジェクトは連想配列のほとんどのニーズをカバーしますが、キーの順序や反復処理が重要な場合、ES6で導入された`Map`オブジェクトがより良い代替手段を提供します。`Map`はキーの順序を保持し、より広範なデータ型をキーとして受け入れ、反復処理やサイズ取得のための便利なメソッドを含んでいます。これらの利点にもかかわらず、従来のオブジェクト構文はそのシンプルさと、多くの一般的なシナリオでの使用の容易さから人気があります。
