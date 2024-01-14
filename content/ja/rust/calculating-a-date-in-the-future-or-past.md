---
title:    "Rust: 「未来または過去の日付を計算する」"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## なぜ

プログラミングをする上で、過去や未来の日付を計算する必要があることがよくあります。そのため、今回はRustでどのように簡単に日付を計算するかを紹介したいと思います。

## 使い方

日付を計算するには、chronoというRustの日時ライブラリを使用します。まずは、依存関係にchronoを追加します。

```Rust
[dependencies]
chrono = "0.4"
```

続いて、計算したい日付をchronoで取得します。例として、今日の日付を取得する方法を紹介します。

```Rust
use chrono::{Local, Datelike};

let today = Local::now();
let year = today.year();
let month = today.month();
let day = today.day();
```

今日の日付が取得できましたので、その日付を基に一週間後の日付を計算します。

```Rust
let next_week = today + chrono::Duration::weeks(1);
```

これで、今日から一週間後の日付が計算されました。さらに、特定の日付や月の初日など、より細かい計算も可能です。

## ディープダイブ

前述したように、chronoライブラリは日時を取得するときに便利ですが、計算する際にも様々なメソッドが用意されています。例えば、特定の曜日を取得する方法や、UTCとの変換方法などがあります。詳しくは公式ドキュメントを参照してください。

## もっと見る

- chrono公式ドキュメント: https://docs.rs/chrono/0.4.9/chrono/