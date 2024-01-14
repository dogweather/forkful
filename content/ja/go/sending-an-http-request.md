---
title:                "Go: 「httpリクエストの送信」"
simple_title:         "「httpリクエストの送信」"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## なぜ

HTTPリクエストを送信する理由はさまざまですが、主な目的はウェブサイトやAPIとの通信です。Go言語を使用すると、簡単かつ高速にHTTPリクエストを送信できます。

## 方法

まず、Go言語でHTTPリクエストを送信するためには、`net/http`パッケージをインポートする必要があります。次に、`net/http`パッケージ内の`http.Get()`関数を使用して、リクエストを送信します。

例えば、GoogleのウェブサイトにGETリクエストを送信するコードを以下に示します。

```Go
package main

import (
    "fmt"
    "io/ioutil"
    "net/http"
)

func main() {
    res, err := http.Get("https://www.google.com")
    if err != nil {
        panic(err)
    }
    defer res.Body.Close()

    body, err := ioutil.ReadAll(res.Body)
    if err != nil {
        panic(err)
    }
    fmt.Println(string(body))
}
```

上記のコードでは、`http.Get()`関数を使用してGoogleのウェブサイトにGETリクエストを送信し、その結果を`res`変数に保存しています。`defer`キーワードを使用することで、リクエストの処理が完了すると自動的にリソースを解放することができます。そして、`ioutil.ReadAll()`関数を使用して`res.Body`からデータを読み取り、文字列として出力しています。

実際に実行すると、以下のような結果が得られます。

```Go
<!doctype html><html itemscope="" itemtype="http://schema.org/WebPage" lang="ja"><head><meta content="テキストから自動翻訳します。" name="description"><meta content="Google" name="author"><meta content="noodp" name="robots"><meta content="text/html; charset=UTF-8" http-equiv="Content-Type"><meta content="/images/branding/googleg/1x/googleg_standard_color_128dp.png" itemprop="image"><title>Google</title><script nonce="rxKv1udJgVSIM0oImxRCoQ==">(function(){window.google={kEI:"XLChXbfSI8bc1wL-wrzQDA",kEXPI:"0,1353746,2015,32894,757,569,117,2558,2252,344,127,1324,304,51,582,1371,2,180,148,385,26,776,52,987,489,753,3,219,243,4,2,2,2,2,4103888,26430019,3299392,1294,2,3287,536,1,2435,6516,1,2262,2,62,160,563,1473,591,4,461,224,2,5,2,2,2,2,8,2,2,2,2,2,10,2081751,277,338025,153,50092,8,15,3330849,1339,3299392,443,3330982,3330988,3,1114,627,1618,3,3317933,59,3209471,1245,4099528,200,56,2,1226,26,2864,50,4617,1059,1573,1695,52,10,1123,376,2,28,850,4413,2,283,20,2095,2,2194,735,869,1347,1129,26,17,85,1164,2,10,763,4860,29,1272827,1344097,1149,1588,603,941,391,1340,3871,2,21,640,4,2,2005,1157,7,1024,2115,4,4,2,10,267,9,3922,459,197,2,401,112,1057,490,65,75,2,2,1,...