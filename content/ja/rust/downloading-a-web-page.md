---
title:                "ウェブページのダウンロード"
html_title:           "Rust: ウェブページのダウンロード"
simple_title:         "ウェブページのダウンロード"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## これは何？なんでやるの？
ウェブページをダウンロードすることは、インターネットからコンピューターに情報を取得することです。プログラマーがこれを行う理由は、ウェブスクレイピングやデータマイニングなど、さまざまなタスクを実行するために必要なデータを収集するためです。

## 方法：
```Rust
use std::io::Read;
use reqwest::Client;

fn main() {
    let mut response = Client::new().get("https://www.example.com").send().unwrap();
    let mut body = String::new();
    response.read_to_string(&mut body).unwrap();

    println!("Response: {}", response.status());
    println!("Body: {}", body);
}
```

出力：
```
Response: 200 OK
Body: <!DOCTYPE html>
<html>
<head>
<title>Example Domain</title>
...
```

## 詳しく見る：
- ウェブページをダウンロードする前に、エンドポイントにリクエストを送信してレスポンスを受け取る必要があります。今回は、`reqwest`クレートを使用していますが、`curl`や`wget`などの代替ツールもあります。
- ダウンロードする際には、通常HTTPやHTTPSプロトコルを使用しますが、独自のプロトコルを作成して使用することもできます。
- ウェブページをダウンロードするというタスクは、ウェブスクレイピングやデータマイニングなど、多くの応用分野で必要とされます。

## 関連リンク：
- RustでのHTTPクライアント `reqwest`のドキュメント：https://docs.rs/reqwest/
- ウェブスクレイピングについての一般的な情報：https://www.crummy.com/software/BeautifulSoup/bs4/doc/