---
title:    "Rust: 文字列を小文字に変換する"
keywords: ["Rust"]
---

{{< edit_this_page >}}

# なぜ
 Rustで文字列を小文字に変換することが重要なのか、その理由を説明します。

## なぜ文字列を小文字に変換するか
文字列を小文字に変換することは、単純に表示の問題ではありません。一つの理由としては、ユーザーが入力した文字列を処理する際に、大文字と小文字の違いを無視する必要がある場合があります。また、文字列の比較や検索を行う際にも、大文字と小文字を区別することができなくなる可能性があります。そのため、文字列を小文字に統一することで、これらの問題を回避することができるのです。

## 方法
まず、Rustでは文字列の小文字への変換を行うために、`to_lowercase()`というメソッドを使用します。これを使うことで、文字列内の全ての文字を小文字に変換することができます。例えば、次のようなコードを実行すると、入力された文字列を小文字に変換して出力します。

```Rust
let input = "Hello World";
let lower_case = input.to_lowercase();

println!("{}", lower_case);
```

上記のコードの実行結果は、`hello world`となります。

また、もし変換結果を新しい文字列として取得したい場合は、`to_lowercase()`メソッドではなく、`to_lowercase()`関数を使用します。これにより、変数を新しい小文字の文字列として定義することができます。

```Rust
let input = "Hello World";
let lower_case: String = input.to_lowercase();

println!("{}", lower_case);
```

上記のコードでも同様に、`hello world`という文字列が出力されます。

## 深堀り
`to_lowercase()`メソッドでは、ASCII文字列のみを小文字に変換することができます。しかし、Unicode文字列の場合は異なります。Unicode文字列の小文字への変換には、`to_lowercase()`メソッドとは別に、`fold()`や`map()`といったメソッドを組み合わせる必要があります。

例えば、次のようなコードでUnicode文字列を小文字に変換することができます。

```Rust
let input = "Hello 世界";
let lower_case: String = input.chars().map(|c| c.to_lowercase()).collect();

println!("{}", lower_case);
```

上記のコードの実行結果は、`hello 世界`となります。

# 参考
- [Rust公式ドキュメント：Strings](https://doc.rust-lang.org/std/string/struct.String.html)
- [Rustで文字列を小文字に変換する方法](https://qiita.com/maroKanatani/items/c1417642f3e16e94a488) 

# また見る
- [Rustで文字列を大文字に変換する方法](https://dev.classmethod.jp/articles/python-rust-uppercase-lowercase/)
- [Rustのメソッドチェーンを理解する](https://qiita.com/yuki_ycino/items/882449c3062fc973e274)