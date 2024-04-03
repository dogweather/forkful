---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:12:45.945263-07:00
description: "TOML\uFF08Tom's Obvious, Minimal\u2026"
lastmod: '2024-03-13T22:44:42.826020-06:00'
model: gpt-4-0125-preview
summary: "TOML\uFF08Tom's Obvious, Minimal Language\uFF09\u306F\u3001\u305D\u306E\u660E\
  \u78BA\u306A\u30BB\u30DE\u30F3\u30C6\u30A3\u30AF\u30B9\u306B\u3088\u308A\u8AAD\u307F\
  \u3084\u3059\u3044\u8A2D\u5B9A\u30D5\u30A1\u30A4\u30EB\u5F62\u5F0F\u3067\u3059\u3002\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u305D\u306E\u30B7\u30F3\u30D7\u30EB\
  \u3055\u3068\u4EBA\u9593\u304C\u8AAD\u3081\u308B\u7279\u6027\u306E\u305F\u3081\u3001\
  \u7279\u5B9A\u306E\u6587\u8108\u3067XML\u3084JSON\u306E\u3088\u3046\u306A\u5F62\u5F0F\
  \u3088\u308A\u3082\u512A\u308C\u305F\u9078\u629E\u80A2\u3068\u3057\u3066\u3001\u30A2\
  \u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u306E\u8A2D\u5B9A\u30D5\u30A1\u30A4\u30EB\
  \u306B\u3053\u308C\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002."
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
