---
title:                "ウェブページのダウンロード"
html_title:           "Bash: ウェブページのダウンロード"
simple_title:         "ウェブページのダウンロード"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Fish Shellによるウェブページのダウンロード

## 何となぜ？

ウェブページのダウンロードは、そのページのHTMLのコピーを取得するプロセスです。これを行う理由は、オフライン使用やデータ解析、あるいはバックアップ目的などがあります。

## やり方

ここでは有名なコマンドラインツールである`curl`を使います。以下に示すのはFish Shellで使用するコード例とその出力例です。

```Fish Shell
function download_page
    set url $argv[1]　# 第一引き数をURLとして取得
    set file $argv[2] # 第二引き数を保存ファイル名として取得

    curl $url -o $file
    echo "ダウンロード完了: $file"
end
```

この関数にURLと保存したいファイル名を与えると、指定したウェブページがダウンロードされます。

## ディープダイブ

1. **歴史的な文脈**

`curl`は、1997年にDaniel Stenbergによって開発され、公に提供されました。それ以来、インターネット上のリソースを操作するツールとして非常に人気があります。

2. **代替手段**

`wget`や`httpie`など、ウェブページをダウンロードする他の方法も存在します。また、Node.jsやPythonといったプログラミング言語を使ってもウェブページをダウンロードすることが可能です。

3. **実装の詳細**

`curl`はコマンドラインからウェブリソースを取得する処理を実装しています。`-o`オプションにより、ダウンロードしたデータを指定のファイルに保存することが可能です。

## 参考書

1. [Fish Shell公式ドキュメンテーション](https://fishshell.com/docs/current/index.html)
2. [Curl公式ドキュメンテーション](https://curl.se/docs/manual.html)
3. [Getting Started with HTTPie](https://httpie.io/docs#introduction)
4. [Wget man page](https://www.gnu.org/software/wget/manual/wget.html)