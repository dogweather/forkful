---
title:                "ウェブページをダウンロードする"
html_title:           "Bash: ウェブページをダウンロードする"
simple_title:         "ウェブページをダウンロードする"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 何が必要で、なぜ？

ウェブページをダウンロードするとは、インターネット上のファイルをコンピューターに保存することです。プログラマーは、自分のコードをテストするためにダウンロードしたり、オンラインから必要なデータを収集するためにダウンロードを行います。

## 方法：

```Bash
# URLからウェブページをダウンロードする
wget URL

# ファイルを保存する際に名前を変更する
wget -O ファイル名 URL

# ウェブページをダウンロードする際にプロキシサーバーを使用する
wget -e use_proxy=yes -e http_proxy=プロキシアドレス URL
```

## 詳細をさぐろう

ウェブページのダウンロードは、インターネットの発展とともに生まれた機能です。プログラマーがウェブページをダウンロードする方法は様々ありますが、Bashを使用することで簡単かつ迅速に行うことができます。また、curlやwget以外にもPythonやJavaなどのプログラミング言語を使用してダウンロードすることも可能です。

## 関連情報

- [【2分で解説】BashでWebページをダウンロードする方法](https://qiita.com/takumi_kusu/items/d7c5e86e3476853d7b6d)
- [Bashでローカルファイルをダウンロードする方法](https://androck.co.jp/2013/05/bash_curl_download/)
- [Pythonを使用してウェブページをダウンロードする方法](https://www.quora.com/How-do-I-download-a-web-page-using-Python)