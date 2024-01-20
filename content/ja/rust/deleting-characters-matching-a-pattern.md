---
title:                "パターンに一致する文字を削除する"
html_title:           "C: パターンに一致する文字を削除する"
simple_title:         "パターンに一致する文字を削除する"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 何となぜ？
パターンに一致する文字を削除するとは、文字列から特定のパターンに一致するすべての文字を取り除く処理のことを指します。プログラマーがこれを行う理由は、データクレンジングや不要な文字の除去、テキスト変換など、さまざまなシナリオで有用だからです。

## どうやって？
Rust 言語を用いてパターンに一致する文字を削除する簡単な例を示します。次のコードでは、文字列から数字を削除します。

```Rust
fn main() {
    let s = "abc123def456";
    let s_without_digits: String = s.chars().filter(|c| !c.is_numeric()).collect();
    println!("{}", s_without_digits);
}
```
このコードを実行すると、「abc123def456」から数字が削除され、「abcdef」という文字列が出力されます。

## ディープダイブ
古くから存在しているこの操作は、Rust スタイルでは特に慎重に扱われます。他の言語では新しい文字列を生成せずに元の文字列を変更する「ディストラクティブ」というアプローチが採用されることがありますが、Rust ではそのような操作を避け、新しい文字列を生成します。

パターンに一致する文字の削除には他の方法もあります。あるいは、正規表現ライブラリを使用して高度なパターンマッチングを行うことが可能です。しかし、この記事の範囲内で扱う単純なケースでは、「filter」メソッドを用いる方法が最も直感的で効率的です。

もちろん、文字列の操作にはメモリ使用量とパフォーマンスが関連します。Rust の「filter」操作はイテレータを使用し、新しい文字列を効率的に生成します。

## 関連情報
* 文字列操作の詳細：[公式ドキュメンテーション](https://doc.rust-lang.org/book/ch08-02-strings.html)
* 正規表現によるパターンマッチング：[regex crate](https://docs.rs/regex/1.3.9/regex/)
* Rust コミュニティーのディスカッション：[Rust subreddit](https://www.reddit.com/r/rust/)