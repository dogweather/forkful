---
title:                "ウェブページをダウンロードする"
html_title:           "Gleam: ウェブページをダウンロードする"
simple_title:         "ウェブページをダウンロードする"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# なぜダウンロードする必要があるのか？

ウェブページのダウンロードは、あなたのプログラムが必要とするデータを入手するために必要です。例えば、スクレイピングやデータ収集のために、特定のWebページから情報を収集する場合に使用されます。

## ダウンロードする方法

```gleam
pub fn main() {
  let url = "https://example.com";
  let response = http.Client.get(url);
  match response {
    Ok(resp) -> {
      println(show(resp.body))
    }
    Err(err) -> {
      println(err)
    }
  }
}
```

上記のようなコードを使用して、Gleamを使用して特定のURLからデータをダウンロードすることができます。`http`モジュールの`Client`型を使用してGETリクエストを作成し、レスポンスを取得し、レスポンスの本文を表示することができます。

```gleam
成功
<!DOCTYPE html>
<html>
<head>
  <title>Example Domain</title>
  <meta charset="utf-8" />
  <meta http-equiv="Content-type" content="text/html; charset=utf-8" />
  <meta name="viewport" content="width=device-width, initial-scale=1" />
  <style type="text/css">
    body {
      background-color: #f0f0f2;
      margin: 0;
      padding: 0;
      font-family: -apple-system, system-ui, BlinkMacSystemFont, "Segoe UI", "Roboto", "Helvetica Neue
```

## さらに詳しく

ウェブページをダウンロードする方法はさまざまありますが、Gleamを使用するとかなり簡単にできます。`http`モジュールには、メソッドやヘッダーの設定など、さまざまな機能が用意されています。さらに、`httpc`ライブラリを使用することで、GleamのHTTPリクエストをより高度に操作することもできます。

# もっと詳しく知りたい場合は

- Gleamの公式ドキュメント: https://gleam.run/
  - `http`モジュールの詳細: https://gleam.run/modules/http.html
  - `httpc`ライブラリの詳細: https://hexdocs.pm/httpc/readme.html