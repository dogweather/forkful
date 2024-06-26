---
date: 2024-01-20 17:43:47.614029-07:00
description: "How to / \u65B9\u6CD5 Bash\u3067web\u30DA\u30FC\u30B8\u3092\u30C0\u30A6\
  \u30F3\u30ED\u30FC\u30C9\u3059\u308B\u7C21\u5358\u306A\u65B9\u6CD5\u306F`curl`\u3084\
  `wget`\u3092\u4F7F\u3046\u3053\u3068\u3067\u3059\u3002\u4EE5\u4E0B\u306F\u5177\u4F53\
  \u7684\u306A\u4F8B\u3067\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T22:38:41.886499-06:00'
model: gpt-4-1106-preview
summary: "How to / \u65B9\u6CD5 Bash\u3067web\u30DA\u30FC\u30B8\u3092\u30C0\u30A6\u30F3\
  \u30ED\u30FC\u30C9\u3059\u308B\u7C21\u5358\u306A\u65B9\u6CD5\u306F`curl`\u3084`wget`\u3092\
  \u4F7F\u3046\u3053\u3068\u3067\u3059\u3002\u4EE5\u4E0B\u306F\u5177\u4F53\u7684\u306A\
  \u4F8B\u3067\u3059\u3002"
title: "\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u306E\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9"
weight: 42
---

## How to / 方法
Bashでwebページをダウンロードする簡単な方法は`curl`や`wget`を使うことです。以下は具体的な例です。

```Bash
# curlを使ってwebページをダウンロードし、ファイルに保存する
curl http://example.com -o example_page.html

# wgetを使ってwebページをダウンロードする
wget http://example.com
```

実行結果のサンプル:

```Bash
$ curl http://example.com -o example_page.html
  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
100  1256  100  1256    0     0   6562      0 --:--:-- --:--:-- --:--:--  6579

$ wget http://example.com
--2023-04-02 12:00:00--  http://example.com/
Resolving example.com (example.com)... 93.184.216.34
Connecting to example.com (example.com)|93.184.216.34|:80... connected.
HTTP request sent, awaiting response... 200 OK
Length: unspecified [text/html]
Saving to: ‘index.html’

    [ <=>                                   ] 1,256       --.-K/s   in 0s      

2023-04-02 12:00:01 (30.7 MB/s) - ‘index.html’ saved [1256]
```

## Deep Dive / 徹底解析
最初、UNIX環境でウェブコンテンツをダウンロードするためにはFTPや独自のプロトコルを使ってファイルを手動で取り込む必要がありました。しかし、90年代半ばになると`curl`と`wget`が登場し、HTTPプロトコル経由でのダウンロードが一般化しました。

`curl`は多くのプロトコルをサポートし、データ送信にも使えるなどより複雑な操作が可能です。一方、`wget`は再帰的ダウンロードやオフライン閲覧のためのウェブサイトのミラーリングに特化しています。

また、プログラミング言語固有のライブラリやツールも存在します。たとえば、Pythonの`requests`ライブラリや、Node.jsの`axios`などがあります。これらは一般的にはアプリケーション内で組み込んで使用します。

## See Also / 関連情報
- `curl`の公式ドキュメント: https://curl.se/docs/
- `wget`のマニュアル: https://www.gnu.org/software/wget/manual/wget.html
- `requests`ライブラリ（Python): https://docs.python-requests.org/
- `axios`ライブラリ（Node.js): https://axios-http.com/docs/intro
