---
title:    "Rust: デバッグ出力の印刷"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## なぜ

デバッグ出力を印刷することについて、私たちがその問題を知っているとき、私たちはなぜそれをする必要があるのか疑問に思うかもしれません。ただし、デバッグ出力を印刷することは、コーディング中に生じるエラーの原因を追跡し、バグを修正するのに非常に役立ちます。簡単なステップで実装できる上、効率的なデバッグを可能にします。

## 方法

デバッグ出力を印刷するには、Rustの `println!` マクロを使用します。たとえば、次のように使用することができます。

```Rust
println!("Debug output: {}", variable);
```

このコードは、"Debug output: "という文字列と、`variable` の値を出力します。

また、複数の変数を出力する必要がある場合は、次のように書くこともできます。

```Rust
println!("Debug output: {}, {}, {}", variable1, variable2, variable3);
```

## 深堀り

デバッグ出力を印刷する際には、フォーマット文字列を使ってより詳細な情報を表示することもできます。たとえば、変数の型を指定することで、コードの実行状況をより明確にすることができます。フォーマット文字列は、 `{}` の代わりに `{:?}` を使用することで指定することができます。

```Rust
println!("Debug output: {:?} has the value {}", variable, variable);
```

また、デバッグ出力のフォーマットをカスタマイズすることもできます。詳しくは、公式ドキュメントの [フォーマット](https://doc.rust-lang.org/std/fmt/index.html) 部分を参照してください。

## 参考

- Rust公式ドキュメント: [デバッグ出力の印刷](https://doc.rust-lang.org/book/ch01-05-control-flow.html#printing-values-with-println)
- Qiita: [Rustではじめるデバッグ出力](https://qiita.com/hinohikochi/items/319fc8bdac134e31a509)

## 関連リンク

- [マークダウン記法とは？](https://qiita.com/tbpgr/items/989c6badefff69377da7)
- [Rustの基本：関数](https://qiita.com/nariya/items/93301650e67b8ce5a495)