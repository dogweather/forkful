---
title:                "文字列の補間"
html_title:           "Arduino: 文字列の補間"
simple_title:         "文字列の補間"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/interpolating-a-string.md"
---

{{< edit_this_page >}}

# ジャバスクリプトでの文字列の補間
## 何 - なぜ？(What & Why?)
文字列の補間とは、固定の文字列に変数の値を埋め込んで一つの文字列を生成することです。プログラマーがこの方法を使用する理由は、従来の文字列連結よりもより簡潔で読みやすいコードを書けるからです。

## 使い方(How to)
補間を使った文字列の生成は、バッククォート（`）を使います。${}の中に変数を挿入します。具体的なプログラム例を見てみましょう：

```Javascript
let name = "Taro";
let message = `こんにちは、${name}さん！`;
console.log(message);
```

上記のプログラムの出力結果：
```
こんにちは、Taroさん！
```

## 詳細情報(Deep Dive)
1. **歴史的な文脈：** JavaScriptのバージョンES6から文字列の補間が導入されました。それ以前は、`+` operatorを使った文字列の連結が一般的でした。

2. **代替手段：** 今でも `+` や `concat()`メソッドによる文字列連結を使用することも可能です。

```Javascript
// + operator
let name = "Taro";
let message = "こんにちは、" + name + "さん！";
console.log(message);

// concat() method
let message = "こんにちは、".concat(name, "さん！");
console.log(message);
```
3. **実装の詳細：** 文字列の補間は、内部的には文字列と変数を連結する操作を行います。特殊な記号${}を使うことで、JavaScriptエンジンはこれを認識し、適切な値で置き換えます。

## 関連資料(See Also)
- [MDN Web Docs: Template Literals](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Template_literals)