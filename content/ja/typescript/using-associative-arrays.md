---
title:                "連想配列の使用"
date:                  2024-01-30T19:13:24.925182-07:00
model:                 gpt-4-0125-preview
simple_title:         "連想配列の使用"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
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
