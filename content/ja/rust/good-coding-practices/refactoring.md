---
title:                "リファクタリング"
aliases: - /ja/rust/refactoring.md
date:                  2024-01-26T03:37:16.213080-07:00
model:                 gpt-4-0125-preview
simple_title:         "リファクタリング"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/refactoring.md"
---

{{< edit_this_page >}}

## 何となぜ？

リファクタリングとは、既存のコンピュータコードの構造を変更する（ファクタリングの変更）プロセスですが、その外部の振る舞いは変更しません。プログラマーは、ソフトウェアの非機能属性を改善するためにこれを行います。例えば、可読性の向上、複雑さの削減、保守性の向上や、拡張性を向上させるためにより表現力のある内部アーキテクチャやオブジェクトモデルを作成します。

## 方法：

Rust言語の簡単なコード片をリファクタリングして、よりイディオマティックで保守しやすくしましょう。整数のベクタの合計を計算する関数から始めます：

```rust
fn sum(vec: &Vec<i32>) -> i32 {
    let mut sum = 0;
    for i in vec {
        sum += i;
    }
    sum
}

fn main() {
    let numbers = vec![1, 2, 3, 4, 5];
    println!("合計は {}", sum(&numbers));
}
```

出力：
```
合計は 15
```

では、イテレータと`fold`メソッドを活用することで、もっとイディオマティックなRustを使用してリファクタリングしてみましょう：

```rust
fn sum(vec: &[i32]) -> i32 {
    vec.iter().fold(0, |acc, &x| acc + x)
}

fn main() {
    let numbers = vec![1, 2, 3, 4, 5];
    println!("合計は {}", sum(&numbers));
}
```

出力は変わりません ─ 依然として`15`です ─ しかし、リファクタリングされたバージョンはよりクリーンで、Rustの長所である借用やイテレータメソッドを活用しています。

## 深堀り

リファクタリングはSmalltalkコミュニティにルーツを持ち、Martin Fowlerの著書「Refactoring: Improving the Design of Existing Code」によってJavaの世界で普及しました。その原則は普遍的であり、安全性と並行性が最優先されるRustにも適用されます。Rustはコンパイル時に問題を捕捉することで堅牢なコードの記述を促進しますので、リファクタリング中にもRustコンパイラが安全網として機能します。

手動でのリファクタリングへの代替手段には、コードのフォーマットに`rustfmt`、リンティングに`clippy`を使うなどの自動化ツールがあります。これらはコードのよりイディオマティックな書き方を提案できます。しかし、深いリファクタリングはしばしばコードの設計に対する洞察に基づく理解を必要とし、これらのツールでは完全に自動化できません。

Rustにおけるリファクタリングは、型の使用を改善すること、ライフタイムを効果的に活用すること、不必要なアロケーションを削減すること、また必要に応じて`Arc<Mutex<T>>`のような並行性パターンを使用することなどに焦点を当てることが一般的です。また、`unwrap()`からより表現豊かなエラーハンドリングへの移行、つまり`Result<T, E>`の使用への移行もよくあります。

## 関連情報

Rustでのリファクタリングをさらに深く掘り下げるには：

- The Rust Book: https://doc.rust-lang.org/book/
- Rust by Example: https://doc.rust-lang.org/rust-by-example/
- Clippy、Rustのリンティングツール: https://github.com/rust-lang/rust-clippy
- 「Refactoring: Improving the Design of Existing Code」by Martin Fowler: https://martinfowler.com/books/refactoring.html
