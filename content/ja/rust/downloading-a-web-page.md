---
title:                "ウェブページのダウンロード"
html_title:           "Bash: ウェブページのダウンロード"
simple_title:         "ウェブページのダウンロード"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 何となぜ?

ウェブページをダウンロードするとは、ウェブサーバーからページのデータを取得して自分のマシンに保存することを意味します。これを行う理由は、コンテンツをオフラインで利用できるようにすること、またはデータ分析のためにウェブページのコンテンツを解析することがあります。

## 自分で試してみよう:

```rust
use reqwest;
use std::fs;

#[tokio::main]
async fn main() -> Result<(),reqwest::Error> {
    let content = reqwest::get("https://www.google.com/")
        .await?
        .text()
        .await?;
    fs::write("google.html", &content).expect("Unable to write file");
    println!("Download successful!");
    Ok(())
}
```

このコードを実行すると、「Download successful!」が出力されます。また、「google.html」という名前の新しいファイルが作成され、その中に取得したウェブページのコンテンツが保存されます。

## ディープダイブ:

ウェブページのダウンロードは1990年ごろから行われてきました。それ以来、HTTPクライアント、ブラウザーなどのツールが開発され、この処理を大幅に簡単にしました。Rust言語もHTTPクライアントライブラリ（たとえば`reqwest`や`hyper`）など、ウェブページのダウンロードを容易かつ効率的に行うための強力なツールを提供しています。

ダウンロードの代わりにウェブページをキャッシュする方法もあります。これはパフォーマンス向上やデータの保存のために良く行われますが、それには通常、特別な設定や専門的なツールが必要となります。

## 参考資料:

- [reqwest GitHub](https://github.com/seanmonstar/reqwest)
- [hyper GitHub](https://github.com/hyperium/hyper)
- [Rust 公式ドキュメンテーション](https://www.rust-lang.org/)