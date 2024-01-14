---
title:    "TypeScript: デバッグ出力の印刷"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## なぜデバッグ出力をプリントするのか

コンピュータプログラムを書く際には、何かがうまく動かない場合があります。その原因を特定するには、デバッグが欠かせません。デバッグとは、プログラムの実行中に出力される情報を確認し、問題の箇所を見つける作業です。そこで、デバッグ出力をプリントすることで、プログラムの動作を確認し、問題の原因を特定することができます。

## デバッグ出力のプリント方法

TypeScriptでは、```console.log()```メソッドを使用してデバッグ出力を行うことができます。例えば、次のコードを実行すると、コンソールに文字列 ```"Hello World!"``` が出力されます。

```TypeScript 
console.log("Hello World!"); 
```

また、変数の値を確認する際にも便利です。次のコードでは、変数 ```num``` の値をコンソールに出力しています。

```TypeScript 
let num = 10; 
console.log(num); // Output: 10 
```

さらに、複数の値を出力することもできます。次のコードでは、変数 ```name``` と変数 ```age``` の値をコンソールに出力しています。

```TypeScript 
let name = "John"; 
let age = 30; 
console.log(name, age); // Output: John 30 
```

## デバッグ出力の深堀り

デバッグ出力が与える情報は、問題の特定に役立ちます。しかし、多くの情報が出力される場合、その中から問題の原因を特定することは容易ではありません。そのため、適切なタイミングでデバッグ出力を行うことが重要です。また、コンソール以外にも、ブラウザのデバッガーツールや外部のログファイルに出力することもできます。

## さらに参考になるリンク

- [TypeScript公式ドキュメント](https://www.typescriptlang.org/docs/)
- [デバッグの基本](https://qiita.com/shoyan/items/9d33f05783d797df6a9f)
- [デバッグ方法の選択](https://www.sejuku.net/blog/66045)

## 参考リンク

- [Markdownガイド](https://gist.github.com/mignonstyle/083c9e1651d7734f84c99b8cf49d57fa)
- [Thonny - デバッグの基本](https://thonny.org/ja/learning-basics/debugging.html)
- [Visual Studio Code - デバッグの方法](https://code.visualstudio.com/docs/editor/debugging)