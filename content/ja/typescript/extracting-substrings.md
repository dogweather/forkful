---
title:    "TypeScript: サブストリングの抽出"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

# なぜサブストリングを抽出するのか

サブストリングを抽出することは、文字列をより小さな部分に分割するための便利な方法です。これにより、文字列をより簡単に処理することができます。例えば、特定のキーワードを含む部分文字列を抽出したり、文字列の一部を置き換えたりすることができます。

## 方法

TypeScriptを使用してサブストリングを抽出する方法を見ていきます。まずは、サブストリングを抽出したい文字列を指定します。

```TypeScript
let str = "こんにちは、私の名前は太郎です。";
```

次に、`substr()`メソッドを使用して、抽出したいサブストリングを指定します。`substr()`メソッドには、抽出したいサブストリングの開始位置と、終了位置を引数として指定します。

```TypeScript
let name = str.substr(10, 2);
console.log(name); // 結果：太郎
```
上記の例では、`substr()`メソッドを使用して、文字列の10番目から2文字分のサブストリングを抽出しています。

## 深堀り

`substr()`メソッドは、指定した開始位置から、指定した長さのサブストリングを抽出しますが、`substring()`メソッドを使用すると、指定した開始位置から、指定した終了位置までのサブストリングを抽出することができます。また、`slice()`メソッドも同様の機能を持っています。

```TypeScript
let str = "こんにちは、私の名前は太郎です。";
let name = str.substring(10, 12); // または str.slice(10, 12);
console.log(name); // 結果：太郎
```

サブストリングを抽出する際に、正規表現を使用することもできます。正規表現を使用することで、より柔軟な抽出が可能になります。

見るべきは次のとおりです。

## 参考文献

- [String.prototype.substr() - MDN](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/substr)
- [String.prototype.substring() - MDN](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [String.prototype.slice() - MDN](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/slice)

## 関連リンク

- [TypeScript公式サイト](https://www.typescriptlang.org/)
- [正規表現チュートリアル - MDN](https://developer.mozilla.org/ja/docs/Web/JavaScript/Guide/Regular_Expressions)
- [JavaScriptのサブストリングの使い方 - Qiita](https://qiita.com/taisuke-j/items/a6fd91d793546eadf0a8)