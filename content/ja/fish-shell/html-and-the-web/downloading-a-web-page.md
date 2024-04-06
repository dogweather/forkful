---
date: 2024-01-20 17:44:16.844371-07:00
description: "How to: (\u65B9\u6CD5) \u3053\u306E\u30B3\u30DE\u30F3\u30C9\u306F example.com\
  \ \u306E\u5185\u5BB9\u3092 `page.html` \u30D5\u30A1\u30A4\u30EB\u306B\u4FDD\u5B58\
  \u3057\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.518184-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) \u3053\u306E\u30B3\u30DE\u30F3\u30C9\u306F example.com \u306E\
  \u5185\u5BB9\u3092 `page.html` \u30D5\u30A1\u30A4\u30EB\u306B\u4FDD\u5B58\u3057\u307E\
  \u3059\u3002"
title: "\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u306E\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9"
weight: 42
---

## How to: (方法)
```Fish Shell
set url "http://example.com"
curl -o page.html $url
```

このコマンドは example.com の内容を `page.html` ファイルに保存します。

```Fish Shell
cat page.html
```

`page.html` ファイルの内容を表示します。出力はHTMLコードになります。

## Deep Dive (深い潜入)
インターネットの黎明期、ウェブページは手作業でダウンロードされました。今日においては、`curl` や `wget` のようなツールがこのプロセスを自動化し、効率化しています。これらのツールはサーバーからファイルをフェッチし、HTTPプロトコルを利用して通信します。

`curl` は多様なプロトコルに対応しており、開発者から広く信頼されています。`wget` は再帰的ダウンロードが特徴です。どちらもフリーでオープンソースのソフトウェアです。

Fish Shellの文法は直観的で、記述も簡潔です。スクリプトは可読性が高く、効率的なコーディングが可能です。

## See Also (関連情報)
- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [curl project](https://curl.se/)
- [GNU Wget](https://www.gnu.org/software/wget/)
- [HTTP Protocols](https://www.w3.org/Protocols/)
