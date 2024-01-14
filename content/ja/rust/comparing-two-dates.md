---
title:    "Rust: 2つの日付を比較する"
keywords: ["Rust"]
---

{{< edit_this_page >}}

Rust言語で日付を比較する方法

## 起因
日付を比較することは、プログラミングの中でも非常によく行われます。例えば、ブログの投稿日と更新日を比較して、最新の投稿を表示したり、有効期限を過ぎたイベントを非表示にしたりすることができます。しかし、Rust言語を使う場合、日付を比較する方法は他の言語とは少し異なります。そこで、今回はRust言語で日付を比較する方法についてご紹介します。

## 方法
Rust言語では、日付を表すために`DateTime`というデータ型が用意されています。このデータ型は、`Chrono`というライブラリを使用することで利用することができます。まずは、`Chrono`ライブラリをプログラムに追加します。

```Rust
extern crate chrono; // Chronoライブラリを追加

use chrono::{DateTime, Utc}; // データ型DateTimeとタイムゾーンのUtcを使用
```

次に、比較したい日付をそれぞれ`DateTime`のインスタンスとして作成し、そのインスタンスを比較演算子`<`、`>`、`=`で比較することで、日付の大小を判定することができます。

```Rust
let date1: DateTime<Utc> = Utc::now(); // 現在の日付を取得
let date2: DateTime<Utc> = Utc.ymd(2020, 1, 1).and_hms(0, 0, 0); // 2020年1月1日を指定して作成

if date1 > date2 {
    println!("date1 is later than date2");
} else if date1 < date2 {
    println!("date1 is earlier than date2");
} else {
    println!("Both dates are the same");
}
```

上記のコードでは、`Utc.ymd()`メソッドと`and_hms()`メソッドを使用して、年月日と時分秒を指定して`DateTime`のインスタンスを作成しています。また、`now()`メソッドを使用することで現在の日付を取得することができます。

## 詳細を追う
Rust言語では、比較演算子の他にも`DateTime`オブジェクトのメソッドを使用することで、日付の比較や計算を行うことができます。例えば、`date1.month()`や`date2.year()`などを使用することで、日付の特定の部分を取得することができます。また、`date1.succ()`や`date2.pred()`を使用することで、前後の日付を取得することもできます。

詳しくは、Chronoライブラリの公式ドキュメントを参照してください。

## 関連情報
- Chronoライブラリ公式ドキュメント: https://docs.rs/chrono/
- Rust言語公式ドキュメント: https://www.rust-lang.org/ja/
- 日付と時刻を扱うプログラムのコーディングにおける悩みとその解決策: https://tech.atguys.co.jp/tech-blog/date-and-time-in-programming/