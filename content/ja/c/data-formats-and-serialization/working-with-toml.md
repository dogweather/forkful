---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:12:45.945263-07:00
description: "\u3069\u306E\u3088\u3046\u306B\uFF1A C\u3067TOML\u3092\u6271\u3046\u306B\
  \u306F\u3001C\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\u306B\u306F\u3053\u306E\u6A5F\
  \u80FD\u304C\u542B\u307E\u308C\u3066\u3044\u306A\u3044\u305F\u3081\u3001TOML\u30D5\
  \u30A1\u30A4\u30EB\u3092\u89E3\u6790\u3067\u304D\u308B\u30E9\u30A4\u30D6\u30E9\u30EA\
  \u304C\u6700\u521D\u306B\u5FC5\u8981\u306B\u306A\u308A\u307E\u3059\u3002\u300Ctomlc99\u300D\
  \u3068\u3044\u3046\u3001C99\u306E\u305F\u3081\u306E\u8EFD\u91CFTOML\u30D1\u30FC\u30B5\
  \u30FC\u304C\u4EBA\u6C17\u306E\u9078\u629E\u80A2\u3067\u3059\u3002\u4EE5\u4E0B\u306F\
  \u3001\u30B7\u30F3\u30D7\u30EB\u306ATOML\u8A2D\u5B9A\u30D5\u30A1\u30A4\u30EB\u3092\
  \u8AAD\u307F\u8FBC\u3080\u305F\u3081\u306E\u7C21\u5358\u306A\u30AC\u30A4\u30C9\u3067\
  \u3059\uFF1A\u2026"
lastmod: '2024-04-05T21:53:43.618300-06:00'
model: gpt-4-0125-preview
summary: "**\u30B5\u30F3\u30D7\u30EBTOML\u30D5\u30A1\u30A4\u30EB\uFF08`config.toml`\uFF09\
  \uFF1A**."
title: "TOML\u3092\u64CD\u4F5C\u3059\u308B"
weight: 39
---

## どのように：
CでTOMLを扱うには、C標準ライブラリにはこの機能が含まれていないため、TOMLファイルを解析できるライブラリが最初に必要になります。「tomlc99」という、C99のための軽量TOMLパーサーが人気の選択肢です。以下は、シンプルなTOML設定ファイルを読み込むための簡単なガイドです：

まず、`tomlc99`が正しくインストールされ、プロジェクトに適切にリンクされていることを確認してください。

**サンプルTOMLファイル（`config.toml`）：**
```toml
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
```

**このファイルを解析するCコード：**

```c
#include <stdio.h>
#include <stdlib.h>
#include "toml.h"

int main() {
    FILE *configFile;
    configFile = fopen("config.toml", "r");
    if (!configFile) {
        perror("ファイルを開けません");
        return EXIT_FAILURE;
    }

    toml_table_t *config = toml_parse_file(configFile, NULL, 0);
    if (!config) {
        fprintf(stderr, "ファイルの解析エラー\n");
        fclose(configFile);
        return EXIT_FAILURE;
    }

    toml_table_t *database = toml_table_in(config, "database");
    if (database) {
        const char *server = toml_raw_in(database, "server");
        printf("データベースサーバー: %s\n", server);

        toml_array_t *ports = toml_array_in(database, "ports");
        for (int i = 0; i < toml_array_nelem(ports); i++) {
            int64_t port;
            toml_int_at(ports, i, &port);
            printf("ポート %d: %ld\n", i, port);
        }
    }

    toml_free(config);
    fclose(configFile);
    return EXIT_SUCCESS;
}
```

**出力：**
```
データベースサーバー: "192.168.1.1"
ポート 0: 8001
ポート 1: 8001
ポート 2: 8002
```

## 深く掘り下げる
TOMLは、GitHubの共同創設者であるTom Preston-Wernerによって、他の設定ファイル形式の制限に対する反応として作成されました。その目的は、複雑な解析ルールを必要とせずに、人間とコンピュータの両方が読み書きできるように、直接的で曖昧さのないものであることです。Cのエコシステム内では、Rustの`serde_toml`やPythonの`toml`のような高水準言語であるところのネイティブサポートを持つライブラリがあるように、TOMLは第一級の市民ではありません。しかし、C開発者は`tomlc99`のような外部ライブラリに依存する必要がありますが、これはCのミニマリズムとパフォーマンスへの重点を考えると典型的です。

TOMLはその明快さで賞賛されていますが、設定ファイル形式を選択する際には、プロジェクトのニーズを考慮することが重要です。より複雑な構造が必要な場合やウェブAPIとの相互運用が求められるシナリオでは、その増加した複雑さにもかかわらず、JSONやYAMLがより良い適合を提供するかもしれません。TOMLは、必ずしも最も高度なデータ構造が必要なわけではなく、可読性とシンプルさが最優先される設定において輝きます。
