---
title:                "HTMLの解析"
date:                  2024-01-20T15:33:56.051678-07:00
simple_title:         "HTMLの解析"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? - 何となぜ?

HTML解析（パース）とは、HTMLデータから構造情報や内容を取り出すことです。プログラマーはそれを行い、Webページのデータを抽出したり、解析したりするためにそれを行います。

## How to: - 方法

以下は、RustでHTMLを解析するシンプルな例です。必要なライブラリとして`scraper`を使用します。

```Rust
use scraper::{Html, Selector};

fn main() {
    // HTMLデータ
    let html = r#"
        <ul>
            <li>Rust</li>
            <li>Programming</li>
            <li>HTML</li>
        </ul>
    "#;

    let document = Html::parse_document(html);
    let selector = Selector::parse("li").unwrap();

    for element in document.select(&selector) {
        let text = element.text().collect::<Vec<_>>();
        println!("{:?}", text);
    }
}
```

これは出力結果です:

```
["Rust"]
["Programming"]
["HTML"]
```

## Deep Dive - 深掘り

歴史的背景から言うと、HTML解析はウェブの初期から行われている基本的な作業です。RustでHTMLを解析するためのライブラーリはいくつかあり、`scraper`、`html5ever`、`kuchiki`などがあります。`scraper`は`html5ever`に依存していて、高い性能を持っています。`html5ever`はServoブラウザエンジンにも使われており、解析速度が一定なのが特徴です。

実装の面では、`scraper`は`lxml`や`BeautifulSoup`のようなPythonライブラリに影響を受けて設計されています。セレクタはCSSセレクタと同様の文法を利用しているため、Web開発者には親しみやすいはずです。

## See Also - 参照

- `scraper`のドキュメント: [https://docs.rs/scraper](https://docs.rs/scraper)
- `html5ever` GitHub ページ: [https://github.com/servo/html5ever](https://github.com/servo/html5ever)
- Servoブラウザエンジン: [https://github.com/servo/servo](https://github.com/servo/servo)
