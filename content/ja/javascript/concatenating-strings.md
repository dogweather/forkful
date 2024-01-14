---
title:    "Javascript: 文字列の連結"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## なぜ
なぜ私たちは文字列を連結するのでしょうか？文字列を連結する理由をお伝えします。

## 方法
文字列を連結する方法は二つあります。まずは、`+` 演算子を使う方法です。

```Javascript
let str1 = "こんにちは";
let str2 = "、";
let str3 = "世界";

console.log(str1 + str2 + str3);
```

出力結果：
```
こんにちは、世界
```

もう一つの方法は、 `concat()` メソッドを使う方法です。

```Javascript
let str1 = "こんにちは";
let str2 = "、";
let str3 = "世界";

console.log(str1.concat(str2, str3));
```

出力結果：
```
こんにちは、世界
```

## ディープダイブ
文字列を連結するときは、コードの効率性や可読性を考えることが重要です。短い文字列を連結するときには `+` 演算子が便利ですが、長い文字列を連結する場合は、 `concat()` メソッドを使った方が効率的です。また、ES6からはテンプレート文字列を使う方法もあります。

```Javascript
let str1 = "こんにちは";
let str2 = "、";
let str3 = "世界";

// concat() メソッドを使う場合
console.log(str1.concat(str2, str3));

// テンプレート文字列を使う場合
console.log(`${str1}${str2}${str3}`);
```

出力結果：
```
こんにちは、世界
こんにちは、世界
```

## 関連情報
- [String.prototype.concat() - MDN Web Docs](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
- [Template literals - MDN Web Docs](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Template_literals)