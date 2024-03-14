---
date: 2024-01-20 17:43:47.614029-07:00
description: "Web\u30DA\u30FC\u30B8\u306E\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9\u306F\
  \u3001\u30DA\u30FC\u30B8\u306E\u5185\u5BB9\u3092\u30ED\u30FC\u30AB\u30EB\u30DE\u30B7\
  \u30F3\u306B\u4FDD\u5B58\u3059\u308B\u30D7\u30ED\u30BB\u30B9\u3067\u3059\u3002\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u81EA\u52D5\u5316\u3001\u30C7\u30FC\u30BF\u53CE\
  \u96C6\u3001\u307E\u305F\u306F\u30AA\u30D5\u30E9\u30A4\u30F3\u3067\u306E\u95B2\u89A7\
  \u306E\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.368396-06:00'
model: gpt-4-1106-preview
summary: "Web\u30DA\u30FC\u30B8\u306E\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9\u306F\u3001\
  \u30DA\u30FC\u30B8\u306E\u5185\u5BB9\u3092\u30ED\u30FC\u30AB\u30EB\u30DE\u30B7\u30F3\
  \u306B\u4FDD\u5B58\u3059\u308B\u30D7\u30ED\u30BB\u30B9\u3067\u3059\u3002\u30D7\u30ED\
  \u30B0\u30E9\u30DE\u30FC\u306F\u81EA\u52D5\u5316\u3001\u30C7\u30FC\u30BF\u53CE\u96C6\
  \u3001\u307E\u305F\u306F\u30AA\u30D5\u30E9\u30A4\u30F3\u3067\u306E\u95B2\u89A7\u306E\
  \u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
title: "\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u306E\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9"
---

{{< edit_this_page >}}

## What & Why? / 何となぜ？

Webページのダウンロードは、ページの内容をローカルマシンに保存するプロセスです。プログラマーは自動化、データ収集、またはオフラインでの閲覧のためにこれを行います。

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
