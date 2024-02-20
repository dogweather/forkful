---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:13:24.925182-07:00
description: "\u95A2\u9023\u914D\u5217\u3001\u307E\u305F\u306FTypeScript\u3067\u306E\
  \u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\u306F\u3001\u6587\u5B57\u5217\uFF08\u307E\u305F\
  \u306F\u30AD\u30FC\uFF09\u3092\u4F7F\u3063\u3066\u5024\u306E\u30DA\u30A2\u306B\u30A2\
  \u30AF\u30BB\u30B9\u3092\u53EF\u80FD\u306B\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u30FC\u306F\u3053\u308C\u3092\u4F7F\u3063\u3066\u3001\u5F93\u6765\u306E\
  \u914D\u5217\u306B\u6BD4\u3079\u3066\u3088\u308A\u52D5\u7684\u306A\u30C7\u30FC\u30BF\
  \u30A2\u30AF\u30BB\u30B9\u30D1\u30BF\u30FC\u30F3\u3092\u4F7F\u7528\u3057\u3001\u6570\
  \u5024\u30A4\u30F3\u30C7\u30C3\u30AF\u30B9\u306B\u7E1B\u3089\u308C\u308B\u3053\u3068\
  \u306A\u304F\u30C7\u30FC\u30BF\u306E\u69CB\u9020\u5316\u3068\u30A2\u30AF\u30BB\u30B9\
  \u3092\u67D4\u8EDF\u306B\u884C\u3044\u307E\u3059\u3002"
lastmod: 2024-02-19 22:05:00.932934
model: gpt-4-0125-preview
summary: "\u95A2\u9023\u914D\u5217\u3001\u307E\u305F\u306FTypeScript\u3067\u306E\u30AA\
  \u30D6\u30B8\u30A7\u30AF\u30C8\u306F\u3001\u6587\u5B57\u5217\uFF08\u307E\u305F\u306F\
  \u30AD\u30FC\uFF09\u3092\u4F7F\u3063\u3066\u5024\u306E\u30DA\u30A2\u306B\u30A2\u30AF\
  \u30BB\u30B9\u3092\u53EF\u80FD\u306B\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u306F\u3053\u308C\u3092\u4F7F\u3063\u3066\u3001\u5F93\u6765\u306E\u914D\
  \u5217\u306B\u6BD4\u3079\u3066\u3088\u308A\u52D5\u7684\u306A\u30C7\u30FC\u30BF\u30A2\
  \u30AF\u30BB\u30B9\u30D1\u30BF\u30FC\u30F3\u3092\u4F7F\u7528\u3057\u3001\u6570\u5024\
  \u30A4\u30F3\u30C7\u30C3\u30AF\u30B9\u306B\u7E1B\u3089\u308C\u308B\u3053\u3068\u306A\
  \u304F\u30C7\u30FC\u30BF\u306E\u69CB\u9020\u5316\u3068\u30A2\u30AF\u30BB\u30B9\u3092\
  \u67D4\u8EDF\u306B\u884C\u3044\u307E\u3059\u3002"
title: "\u9023\u60F3\u914D\u5217\u306E\u4F7F\u7528"
---

{{< edit_this_page >}}

## 何となぜ？

関連配列、またはTypeScriptでのオブジェクトは、文字列（またはキー）を使って値のペアにアクセスを可能にします。プログラマーはこれを使って、従来の配列に比べてより動的なデータアクセスパターンを使用し、数値インデックスに縛られることなくデータの構造化とアクセスを柔軟に行います。

## どのようにして：

TypeScriptで関連配列を作成して使用することは明快です。基本的な流れをここに示します：

```TypeScript
// 関連配列を宣言
let user: { [key: string]: string } = {};

// データを追加
user["name"] = "Jane Doe";
user["email"] = "jane@example.com";

console.log(user);
```

出力：

```TypeScript
{ name: 'Jane Doe', email: 'jane@example.com' }
```

キーと値のペアをイテレートすることも簡単です：

```TypeScript
for (let key in user) {
    console.log(key + ": " + user[key]);
}
```

出力：

```TypeScript
name: Jane Doe
email: jane@example.com
```

また、複数のデータ型を扱う場合、TypeScriptの型システムが便利です：

```TypeScript
let mixedTypes: { [key: string]: string | number } = {};
mixedTypes["name"] = "John Doe";
mixedTypes["age"] = 30;

console.log(mixedTypes);
```

出力：

```TypeScript
{ name: 'John Doe', age: 30 }
```

## ディープダイブ

TypeScriptで私達が関連配列として参照するものは、本質的にオブジェクトです。歴史的に、PHPのような言語では、関連配列は基本的な型ですが、JavaScript（そして拡張によりTypeScript）ではこの目的にオブジェクトを使用します。このアプローチは強みであり制限でもあります。オブジェクトは、文字列を値に関連付けるための非常に動的な構造を提供しますが、従来の意味での「配列」として使用することを意図したものではありません。例えば、これらのオブジェクトに直接`push`や`pop`のような配列メソッドを使用することはできません。

キーと値の順序付きコレクションが必要で、配列のような操作が必要な場合、TypeScript（および現代のJavaScript）は`Map`オブジェクトを提供します：

```TypeScript
let userMap = new Map<string, string>();
userMap.set("name", "Jane Doe");
userMap.set("email", "jane@example.com");

userMap.forEach((value, key) => {
    console.log(key + ": " + value);
});
```

TypeScriptの型システムとES6機能のような`Map`は強力な代替手段を提供しますが、オブジェクトリテラルがより効率的であるシナリオやJSONデータ構造で作業する場合に、オブジェクトを関連配列として使用する方法を理解することは便利です。それはすべて、仕事に適切なツールを選択することについてです。
